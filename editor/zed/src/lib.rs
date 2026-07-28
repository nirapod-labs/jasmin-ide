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

        // 1. The path in the user's settings, used exactly as written: a build
        //    from an opam switch, a development build, or a copy kept
        //    somewhere specific.
        if let Some(path) = binary.as_ref().and_then(|binary| binary.path.clone()) {
            return Ok(server_command(
                path,
                arguments,
                worktree,
                configured_environment.as_ref(),
            ));
        }

        // 2. The executable on the user's `PATH`.
        if let Some(path) = worktree.which(SERVER_BINARY) {
            return Ok(server_command(
                path,
                arguments,
                worktree,
                configured_environment.as_ref(),
            ));
        }

        // Both remaining steps need a released binary, and only three
        // platforms have one.
        let platform = platform_tag()?;

        // 3. A copy downloaded earlier, if it still matches its digest. A copy
        //    that does not match is deleted by the check, and the download
        //    below replaces it.
        if let Some(expected) = expected_digest(platform) {
            let cached = asset_name(VERSION, platform);
            if std::fs::metadata(&cached).is_ok() && digest::verify_file(&cached, expected).is_ok()
            {
                return Ok(server_command(
                    cached,
                    arguments,
                    worktree,
                    configured_environment.as_ref(),
                ));
            }
        }

        // 4. The release download, also held to the compiled-in digest.
        let path = download_server(language_server_id, platform)?;
        Ok(server_command(
            path,
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

/// Downloads the server for `platform` and returns the path it was saved to,
/// relative to the extension's working directory.
///
/// Two releases are tried, the tag that equals the extension version and then
/// whatever the repository calls latest. A download whose digest does not match
/// is deleted and reported, and no other release is tried against the same
/// expected digest.
fn download_server(language_server_id: &LanguageServerId, platform: &str) -> zed::Result<String> {
    let Some(expected) = expected_digest(platform) else {
        return Err(format!(
            "This build of the Jasmin IDE extension records no SHA-256 for the {platform} \
             language server, so it will not download one. Set `lsp.{SERVER_ID}.binary.path` \
             to a `{SERVER_BINARY}` build of your own."
        ));
    };
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
        if let Err(error) = digest::verify_file(&name, expected) {
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

