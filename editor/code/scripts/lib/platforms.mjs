// The platforms the extension is packaged for, and what a package of each one
// holds.
//
// Two commands share this list: `package-vsix.mjs` builds one package per
// platform, and `package-check.mjs` reads a package back and checks that the
// server inside it is the one that platform runs. A platform added to one of
// them and not the other would produce a package the check does not cover, so
// both read the table below.
//
// `universal` is the package with no server at all: it is the same extension
// for every platform and finds the executable on `PATH`, which is what a user
// who installed the server through opam or a release asset has.

/** The name the server executable has inside the extension's `server/`. */
export const serverExecutable = "jasmin-lsp";

/** The directory inside the extension that holds the bundled server. */
export const serverDirectory = "server";

/** The file in `server/` that holds the digest of the bundled executable. */
export const digestFileName = "SHA256SUMS";

/**
 * Every platform the extension is packaged for.
 *
 * `vsceTarget` is the `--target` value; `os` and `cpu` are what Node reports
 * as `process.platform` and `process.arch`, so a package built on this host
 * can be checked against the host it was built on.
 */
export const platforms = [
  {name: "darwin-arm64", os: "darwin", cpu: "arm64", vsceTarget: "darwin-arm64"},
  {name: "linux-x64", os: "linux", cpu: "x64", vsceTarget: "linux-x64"},
  {name: "linux-arm64", os: "linux", cpu: "arm64", vsceTarget: "linux-arm64"},
  {name: "alpine-x64", os: "linux", cpu: "x64", vsceTarget: "alpine-x64"},
  {name: "alpine-arm64", os: "linux", cpu: "arm64", vsceTarget: "alpine-arm64"},
  {name: "universal", os: undefined, cpu: undefined, vsceTarget: undefined}
];

/** The platforms that bundle a server binary. */
export const bundledPlatforms = platforms.filter((platform) => platform.vsceTarget !== undefined);

/**
 * The platform with this name, or `undefined` when no package is built for it.
 *
 * The match is exact: `linux-x86_64`, the release asset's spelling, is a
 * different name from this table's `linux-x64`, and a script that quietly
 * accepted it would write a package whose target `vsce` rejects.
 */
export function platformByName(name) {
  return platforms.find((platform) => platform.name === name);
}

/** The platform of the host this script runs on, when one is packaged. */
export function hostPlatform() {
  return platforms.find(
    (platform) => platform.os === process.platform && platform.cpu === process.arch
  );
}

/** Reports whether a platform bundles a server into the package. */
export function bundlesServer(platform) {
  return platform.vsceTarget !== undefined && platform.name !== "universal";
}

/** The file name `vsce package` gives a package of this platform. */
export function packageFileName(platform, version) {
  return platform.vsceTarget === undefined
    ? `jasmin-ide-${version}.vsix`
    : `jasmin-ide-${platform.vsceTarget}-${version}.vsix`;
}

/** Where the server executable sits inside a package of this platform. */
export function packagedServerPath(platform) {
  return `${serverDirectory}/${serverExecutable}`;
}

/** Where the digest file sits inside a package of this platform. */
export function packagedDigestPath() {
  return `${serverDirectory}/${digestFileName}`;
}
