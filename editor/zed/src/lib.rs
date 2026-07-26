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

