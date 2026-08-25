//! The Zed extension for the Jasmin language.
//!
//! The extension speaks no LSP itself: it tells Zed which command starts the
//! Jasmin language server and how to start it. Everything else is declared in
//! `extension.toml` and `languages/jasmin/config.toml`.
//!
//! The server is found in four places, in this order: the path in the user's
//! settings, `jasmin-lsp` on the user's `PATH`, a copy downloaded earlier that
//! still matches its recorded digest, and a release download checked against
//! the same digest. Only three platforms have a released server; elsewhere the
//! extension says so and names them.

mod digest;

use std::collections::HashMap;

use zed::serde_json::{Map, Value};
use zed::settings::LspSettings;
use zed::{
    download_file, github_release_by_tag_name, latest_github_release, make_file_executable,
    set_language_server_installation_status, Architecture, Command, DownloadedFileType,
    GithubRelease, GithubReleaseOptions, LanguageServerId, LanguageServerInstallationStatus, Os,
    Worktree,
};
use zed_extension_api as zed;

/// The id of the language server. It must agree with `[language_servers.*]` in
/// `extension.toml`, the `lsp.*` settings table, and
/// `scope_opt_in_language_servers` in the language configuration.
const SERVER_ID: &str = "jasmin-lsp";

/// The executable the extension looks for on the user's `PATH`.
const SERVER_BINARY: &str = "jasmin-lsp";

/// The version of the extension. One version covers the server and both
/// editors, so it is also the version of the server to start and the tag of
/// the release that carries it.
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The platforms that have a released server, spelled as in the asset names.
const SUPPORTED_PLATFORMS: [&str; 3] = ["linux-x86_64", "linux-aarch64", "macos-aarch64"];

/// The lowercase hexadecimal SHA-256 of each released server asset, keyed by
/// the platform tag. The release records these values here; a platform without
/// a row is never downloaded, because an unchecked binary is never run.
const RELEASE_DIGESTS: &[(&str, &str)] = &[];

/// The GitHub repository that publishes the server releases, written as
/// `<owner>/<name>`. It is empty until the release names the repository
/// together with the first digest table, and the download step reports that
/// this build cannot download a server rather than guessing a location.
const RELEASE_REPOSITORY: &str = "";

/// The keys of `lsp.jasmin-lsp.initialization_options` that are passed to the
/// server unchanged. The server takes `arch` and `include` when there is no
/// project file, and nothing else in that table is its business.
const FORWARDED_OPTION_KEYS: [&str; 2] = ["arch", "include"];

struct JasminExtension;

impl zed::Extension for JasminExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> zed::Result<Command> {
        let settings = LspSettings::for_worktree(language_server_id.as_ref(), worktree)?;
        let binary = settings.binary;
        let arguments = binary
            .as_ref()
            .and_then(|binary| binary.arguments.clone())
            .unwrap_or_default();
        let configured_environment = binary.as_ref().and_then(|binary| binary.env.clone());

        // The four places, in order: the setting, `PATH`, a copy downloaded
        // earlier that still matches its digest, and the release download.
        let source = choose_source(
            binary.as_ref().and_then(|binary| binary.path.clone()),
            worktree.which(SERVER_BINARY),
            platform_tag,
            |platform| {
                let cached = asset_name(VERSION, platform);
                let expected = expected_digest(platform)?;
                cached_copy_is_usable(&cached, expected).then_some(cached)
            },
        )?;

        let command = match source {
            ServerSource::Setting(path) | ServerSource::Path(path) | ServerSource::Cached(path) => {
                path
            }
            ServerSource::Release(platform) => download_server(language_server_id, platform)?,
        };
        Ok(server_command(
            command,
            arguments,
            worktree,
            configured_environment.as_ref(),
        ))
    }

    fn language_server_initialization_options(
        &mut self,
        language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> zed::Result<Option<Value>> {
        let settings = LspSettings::for_worktree(language_server_id.as_ref(), worktree)?;
        let Some(options) = settings.initialization_options else {
            return Ok(None);
        };
        let Value::Object(options) = options else {
            return Err(format!(
                "`lsp.{SERVER_ID}.initialization_options` must be a table, such as \
                 {{ \"arch\": \"x86-64\" }}"
            ));
        };
        Ok(Some(Value::Object(forward_initialization_options(
            &options,
        ))))
    }
}

/// The four places the server is taken from, in the order they are tried.
#[derive(Debug, PartialEq, Eq)]
enum ServerSource {
    /// `lsp.jasmin-lsp.binary.path` in the user's settings, as written.
    Setting(String),
    /// `jasmin-lsp` on the user's `PATH`.
    Path(String),
    /// A copy from an earlier download that still matches its digest.
    Cached(String),
    /// The release download, which carries the platform tag to download for.
    Release(&'static str),
}

/// Chooses the first of the four places that can supply the server: the
/// setting, `PATH`, a copy downloaded earlier, and the release download.
///
/// Neither closure runs unless the places before it found nothing, so a
/// configured path costs no platform check and no file system read. That is
/// what makes the order of this function the whole of the lookup order.
fn choose_source(
    setting: Option<String>,
    on_path: Option<String>,
    platform: impl FnOnce() -> zed::Result<&'static str>,
    cached: impl FnOnce(&'static str) -> Option<String>,
) -> zed::Result<ServerSource> {
    if let Some(path) = setting {
        return Ok(ServerSource::Setting(path));
    }
    if let Some(path) = on_path {
        return Ok(ServerSource::Path(path));
    }
    let platform = platform()?;
    match cached(platform) {
        Some(path) => Ok(ServerSource::Cached(path)),
        None => Ok(ServerSource::Release(platform)),
    }
}

/// True when a copy from an earlier download is there and still matches the
/// digest the release records. `digest::verify_file` deletes a copy that does
/// not match, so the release download replaces it and a stale binary is never
/// started.
fn cached_copy_is_usable(path: &str, expected_hex: &str) -> bool {
    std::fs::metadata(path).is_ok() && digest::verify_file(path, expected_hex).is_ok()
}

/// The digest a release download has to match, or the refusal that stops the
/// download before any request is made: a build that records no digest for the
/// platform never downloads a binary it cannot check.
fn download_digest(platform: &str) -> Result<&'static str, String> {
    expected_digest(platform).ok_or_else(|| {
        format!(
            "This build of the Jasmin IDE extension records no SHA-256 for the {platform} \
             language server, so it will not download one. Set `lsp.{SERVER_ID}.binary.path` \
             to a `{SERVER_BINARY}` build of your own."
        )
    })
}

/// Checks a downloaded file against the digest compiled into this build. A
/// file that does not match is deleted by the check before this returns, so it
/// is never made executable, and the error names the file and both digests.
fn accept_download(path: &str, expected_hex: &str) -> Result<(), String> {
    digest::verify_file(path, expected_hex)
        .map_err(|error| format!("the language server download was refused: {error}"))
}

/// Downloads the server for `platform` and returns the path it was saved to,
/// relative to the extension's working directory.
///
/// Two releases are tried, the tag that equals the extension version and then
/// whatever the repository calls latest. A download whose digest does not match
/// is deleted and reported, and no other release is tried against the same
/// expected digest.
fn download_server(language_server_id: &LanguageServerId, platform: &str) -> zed::Result<String> {
    let expected = download_digest(platform)?;
    if RELEASE_REPOSITORY.is_empty() {
        return Err(format!(
            "This build of the Jasmin IDE extension names no repository to download the \
             {platform} language server from. Set `lsp.{SERVER_ID}.binary.path` to a \
             `{SERVER_BINARY}` build of your own."
        ));
    }

    let name = asset_name(VERSION, platform);
    let tag = format!("v{VERSION}");
    set_language_server_installation_status(
        language_server_id,
        &LanguageServerInstallationStatus::Downloading,
    );

    let mut problems = Vec::new();
    for source in [ReleaseSource::Tagged, ReleaseSource::Latest] {
        let release = match source.lookup(&tag) {
            Ok(release) => release,
            Err(error) => {
                problems.push(error);
                continue;
            }
        };
        let Some(asset) = release.assets.iter().find(|asset| asset.name == name) else {
            problems.push(format!(
                "release {} has no asset named {name}",
                release.version
            ));
            continue;
        };
        if let Err(error) =
            download_file(&asset.download_url, &name, DownloadedFileType::Uncompressed)
        {
            problems.push(format!("{name} could not be downloaded: {error}"));
            continue;
        }
        if let Err(error) = accept_download(&name, expected) {
            return Err(failed(language_server_id, error));
        }
        if let Err(error) = make_file_executable(&name) {
            return Err(failed(
                language_server_id,
                format!(
                    "{name} was downloaded and its SHA-256 matches, but it could not be made \
                     executable: {error}"
                ),
            ));
        }
        return Ok(name);
    }

    Err(failed(
        language_server_id,
        format!(
            "the Jasmin language server could not be downloaded from {RELEASE_REPOSITORY}: {}",
            problems.join("; ")
        ),
    ))
}

/// Reports a failure to the installation status Zed shows, and hands the same
/// message back so that the caller can return it.
fn failed(language_server_id: &LanguageServerId, message: String) -> String {
    set_language_server_installation_status(
        language_server_id,
        &LanguageServerInstallationStatus::Failed(message.clone()),
    );
    message
}

/// The two places a release may be found: under the tag that equals the
/// extension version, and under the repository's latest release.
#[derive(Clone, Copy)]
enum ReleaseSource {
    Tagged,
    Latest,
}

impl ReleaseSource {
    /// Looks the release up, without touching the network until called.
    fn lookup(self, tag: &str) -> zed::Result<GithubRelease> {
        match self {
            ReleaseSource::Tagged => github_release_by_tag_name(RELEASE_REPOSITORY, tag),
            ReleaseSource::Latest => latest_github_release(
                RELEASE_REPOSITORY,
                GithubReleaseOptions {
                    require_assets: true,
                    pre_release: false,
                },
            ),
        }
    }
}

/// The command Zed runs for the server.
///
/// The environment is the shell environment of the worktree, so that the
/// server finds `jasminc`, opam and the rest of the user's toolchain. The
/// extension API 0.7.0 has no separate hook for it; the command carries it.
/// Variables from `lsp.jasmin-lsp.binary.env` replace the inherited ones of
/// the same name.
fn server_command(
    command: String,
    args: Vec<String>,
    worktree: &Worktree,
    environment: Option<&HashMap<String, String>>,
) -> Command {
    let mut env = worktree.shell_env();
    if let Some(environment) = environment {
        for (name, value) in environment {
            env.retain(|(inherited, _)| inherited != name);
            env.push((name.clone(), value.clone()));
        }
    }
    Command { command, args, env }
}

/// The platform tag used in release asset names, or an error naming the
/// platforms that have a released server.
fn platform_tag() -> zed::Result<&'static str> {
    match zed::current_platform() {
        (Os::Linux, Architecture::X8664) => Ok("linux-x86_64"),
        (Os::Linux, Architecture::Aarch64) => Ok("linux-aarch64"),
        (Os::Mac, Architecture::Aarch64) => Ok("macos-aarch64"),
        platform => Err(format!(
            "Jasmin IDE has no released language server for {}. Released platforms: {}.",
            describe_platform(platform),
            SUPPORTED_PLATFORMS.join(", ")
        )),
    }
}

/// Spells a platform out for someone reading an error message.
fn describe_platform(platform: (Os, Architecture)) -> String {
    let os = match platform.0 {
        Os::Mac => "macOS",
        Os::Linux => "Linux",
        Os::Windows => "Windows",
    };
    let architecture = match platform.1 {
        Architecture::Aarch64 => "arm64",
        Architecture::X86 => "x86",
        Architecture::X8664 => "x86-64",
    };
    format!("{os} on {architecture}")
}

/// The SHA-256 the release records for `platform`, when this build knows it.
fn expected_digest(platform: &str) -> Option<&'static str> {
    RELEASE_DIGESTS
        .iter()
        .find(|(tag, _)| *tag == platform)
        .map(|(_, digest)| *digest)
}

/// The name of the server asset in a release and of its copy in the extension's
/// working directory: `jasmin-lsp-<version>-<platform>`.
fn asset_name(version: &str, platform: &str) -> String {
    format!("{SERVER_BINARY}-{version}-{platform}")
}

/// Passes through the initialization options the server understands, and drops
/// the rest of the user's table.
fn forward_initialization_options(options: &Map<String, Value>) -> Map<String, Value> {
    let mut forwarded = Map::new();
    for key in FORWARDED_OPTION_KEYS {
        if let Some(value) = options.get(key) {
            forwarded.insert(key.to_string(), value.clone());
        }
    }
    if let Some(delay) = options
        .get("diagnostics")
        .and_then(|diagnostics| diagnostics.get("delay"))
    {
        let mut diagnostics = Map::new();
        diagnostics.insert("delay".to_string(), delay.clone());
        forwarded.insert("diagnostics".to_string(), Value::Object(diagnostics));
    }
    forwarded
}

zed::register_extension!(JasminExtension);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_asset_name_carries_the_version_and_the_platform() {
        assert_eq!(
            asset_name("0.1.0", "macos-aarch64"),
            "jasmin-lsp-0.1.0-macos-aarch64"
        );
        assert_eq!(
            asset_name("0.2.3", "linux-x86_64"),
            "jasmin-lsp-0.2.3-linux-x86_64"
        );
    }

    #[test]
    fn a_platform_is_spelled_out_for_the_unsupported_message() {
        assert_eq!(
            describe_platform((Os::Mac, Architecture::Aarch64)),
            "macOS on arm64"
        );
        assert_eq!(
            describe_platform((Os::Mac, Architecture::X8664)),
            "macOS on x86-64"
        );
        assert_eq!(
            describe_platform((Os::Windows, Architecture::X8664)),
            "Windows on x86-64"
        );
        assert_eq!(
            describe_platform((Os::Linux, Architecture::X86)),
            "Linux on x86"
        );
        assert_eq!(
            SUPPORTED_PLATFORMS.join(", "),
            "linux-x86_64, linux-aarch64, macos-aarch64"
        );
    }

    #[test]
    fn the_known_options_are_forwarded_and_the_rest_are_dropped() {
        let options: Value = zed::serde_json::from_str(
            r#"{
                "arch": "arm",
                "include": { "SECP": "src/jasmin" },
                "diagnostics": { "delay": 500, "unused": true },
                "trace": "verbose"
            }"#,
        )
        .expect("a table of initialization options");
        let options = options
            .as_object()
            .expect("the fixture is a table of initialization options");

        let forwarded = forward_initialization_options(options);
        let expected: Value = zed::serde_json::from_str(
            r#"{
                "arch": "arm",
                "include": { "SECP": "src/jasmin" },
                "diagnostics": { "delay": 500 }
            }"#,
        )
        .expect("the expected table");
        assert_eq!(Value::Object(forwarded), expected);
    }

    #[test]
    fn an_empty_table_forwards_nothing() {
        assert!(forward_initialization_options(&Map::new()).is_empty());
    }

    #[test]
    fn a_digest_known_for_one_platform_is_not_used_for_another() {
        assert_eq!(expected_digest("linux-aarch64"), None);
    }

    /// The SHA-256 of `abc`, the FIPS 180-4 vector the digest tests use, so a
    /// matching file needs no other fixture.
    const SHA256_OF_ABC: &str = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad";

    /// The path only the setting can supply, and the one only `PATH` can, so a
    /// test tells the four places apart by their contents.
    const SETTING: &str = "/opt/jasmin/bin/jasmin-lsp";
    const ON_PATH: &str = "/usr/local/bin/jasmin-lsp";

    #[test]
    fn the_setting_is_used_before_the_other_three_places() {
        let source = choose_source(
            Some(SETTING.to_string()),
            Some(ON_PATH.to_string()),
            || panic!("the setting must end the lookup before the platform is asked"),
            |_| panic!("the setting must end the lookup before the cache is read"),
        )
        .expect("a configured path is a source");
        assert_eq!(source, ServerSource::Setting(SETTING.to_string()));
    }

    #[test]
    fn the_path_is_used_when_no_setting_names_a_server() {
        let source = choose_source(
            None,
            Some(ON_PATH.to_string()),
            || panic!("a binary on PATH must end the lookup before the platform is asked"),
            |_| panic!("a binary on PATH must end the lookup before the cache is read"),
        )
        .expect("a binary on PATH is a source");
        assert_eq!(source, ServerSource::Path(ON_PATH.to_string()));
    }

    #[test]
    fn a_checked_download_is_used_before_the_release_is_fetched_again() {
        let cached = asset_name(VERSION, "linux-x86_64");
        let source = choose_source(
            None,
            None,
            || Ok("linux-x86_64"),
            |platform| {
                assert_eq!(platform, "linux-x86_64");
                Some(asset_name(VERSION, platform))
            },
        )
        .expect("a cached copy is a source");
        assert_eq!(source, ServerSource::Cached(cached));
    }

    #[test]
    fn the_versioned_release_is_the_last_place_tried() {
        let source = choose_source(
            None,
            None,
            || Ok("macos-aarch64"),
            |platform| {
                assert_eq!(platform, "macos-aarch64");
                None
            },
        )
        .expect("the release is a source");
        assert_eq!(source, ServerSource::Release("macos-aarch64"));
        assert_eq!(
            asset_name(VERSION, "macos-aarch64"),
            format!("jasmin-lsp-{VERSION}-macos-aarch64")
        );
    }

    #[test]
    fn an_unsupported_platform_is_reported_only_after_the_setting_and_path() {
        let unsupported = || {
            Err(
                "Jasmin IDE has no released language server for Windows on x86-64. Released \
                 platforms: linux-x86_64, linux-aarch64, macos-aarch64."
                    .to_string(),
            )
        };

        let source = choose_source(Some(SETTING.to_string()), None, unsupported, |_| {
            panic!("no download is considered for a configured path")
        })
        .expect("a configured path works on a platform with no released server");
        assert_eq!(source, ServerSource::Setting(SETTING.to_string()));

        let error = choose_source(None, None, unsupported, |_| {
            panic!("no download is considered for an unsupported platform")
        })
        .expect_err("an unsupported platform is an error once the first two places are empty");
        assert!(error.contains("Windows on x86-64"), "{error}");
        assert!(error.contains("macos-aarch64"), "{error}");
    }

    #[test]
    fn a_cached_copy_that_fails_its_digest_is_deleted_and_the_release_used_instead() {
        let path = scratch_file("cached-copy.bin", b"a server that is not the released one");
        assert!(
            !cached_copy_is_usable(&path, SHA256_OF_ABC),
            "a copy whose digest does not match is not usable"
        );
        assert!(
            std::fs::metadata(&path).is_err(),
            "the refused copy is deleted, so the release download replaces it"
        );

        let source = choose_source(None, None, || Ok("linux-x86_64"), |_| None)
            .expect("the release is what is left once the cache is refused");
        assert_eq!(source, ServerSource::Release("linux-x86_64"));
    }

    #[test]
    fn a_cached_copy_that_matches_its_digest_is_used() {
        let path = scratch_file("matching-copy.bin", b"abc");
        assert!(cached_copy_is_usable(&path, SHA256_OF_ABC));
        assert!(
            std::fs::metadata(&path).is_ok(),
            "a matching copy is kept for the next start"
        );
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn a_platform_with_no_compiled_digest_is_never_downloaded() {
        let error = download_digest("macos-x86_64")
            .expect_err("a platform whose release digest is not recorded is refused");
        assert!(error.contains("records no SHA-256"), "{error}");
        assert!(error.contains("macos-x86_64"), "{error} names the platform");
        assert!(
            error.contains("lsp.jasmin-lsp.binary.path"),
            "{error} says how to use a build of one's own"
        );
    }

    #[test]
    fn a_downloaded_file_that_fails_its_digest_is_refused_and_deleted() {
        let path = scratch_file("downloaded.bin", b"a substituted language server");
        let error = accept_download(&path, SHA256_OF_ABC)
            .expect_err("a download whose digest does not match is refused");
        assert!(error.contains("was refused"), "{error}");
        assert!(
            error.contains(SHA256_OF_ABC),
            "{error} names the expected digest"
        );
        assert!(
            std::fs::metadata(&path).is_err(),
            "a refused download is deleted rather than made executable"
        );
    }

    #[test]
    fn a_downloaded_file_that_matches_its_digest_is_accepted() {
        let path = scratch_file("accepted.bin", b"abc");
        assert!(accept_download(&path, &SHA256_OF_ABC.to_uppercase()).is_ok());
        let _ = std::fs::remove_file(&path);
    }

    /// Writes `contents` to a fresh file under the system temporary directory
    /// and returns its path, so the tests write nothing into the source tree and
    /// do not collide with each other.
    fn scratch_file(name: &str, contents: &[u8]) -> String {
        let directory =
            std::env::temp_dir().join(format!("jasmin-ide-lookup-{}", std::process::id()));
        std::fs::create_dir_all(&directory).expect("a temporary directory for the lookup tests");
        let path = directory.join(name);
        std::fs::write(&path, contents).expect("the test file");
        path.to_str().expect("a UTF-8 path").to_string()
    }
}
