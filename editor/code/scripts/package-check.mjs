import assert from "node:assert/strict";
import { access, mkdir, readFile, readdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { digestOfBytes, parseDigestFile } from "./lib/digest.mjs";
import {
  bundledPlatforms,
  packagedDigestPath,
  packagedServerPath,
  serverDirectory,
  serverExecutable
} from "./lib/platforms.mjs";
import { readZip } from "./lib/vsix.mjs";

const artifactsDirectory = new URL("../.artifacts/", import.meta.url);
const output = new URL("../.artifacts/jasmin-ide.vsix", import.meta.url);
const vsceCli = new URL("../node_modules/.bin/vsce", import.meta.url);

// The files the published extension cannot work without: the bundled client
// entry point, the language configuration and the TextMate grammar.
const requiredEntries = [
  "dist/extension.js",
  "language-configuration.json",
  "package.json",
  "syntaxes/jasmin.tmLanguage.json"
];

// What a package must not carry. A source map is 1.6 MB and names the absolute
// paths of the machine that built it; the sources, the tests, the fixtures and
// the packaging scripts are of no use to an editor that loads the bundle, and
// `node_modules` and `.vscode-test` are build state.
const excludedPatterns = [
  /^node_modules\//u,
  /^src\//u,
  /^scripts\//u,
  /^tests?\//u,
  /^fixtures\//u,
  /^\.vscode-test/u,
  /^\.artifacts\//u,
  /\.map$/u,
  /\.ts$/u
];

await mkdir(artifactsDirectory, {recursive: true});

const listing = spawnSync(fileURLToPath(vsceCli), ["ls", "--no-dependencies"], {
  cwd: fileURLToPath(new URL("..", import.meta.url)),
  encoding: "utf8"
});

if (listing.status !== 0) {
  process.stderr.write(listing.stderr ?? "");
  process.exit(listing.status ?? 1);
}

const listed = listing.stdout
  .split(/\r?\n/u)
  .map((entry) => entry.trim())
  .filter((entry) => entry !== "");

for (const entry of requiredEntries) {
  assert.ok(listed.includes(entry), `vsce ls does not list ${entry}`);
}

for (const entry of listed) {
  for (const pattern of excludedPatterns) {
    assert.ok(
      !pattern.test(entry),
      `vsce ls lists ${entry}, which the package must not carry (it matches ${pattern})`
    );
  }
}

console.log(`vsce ls lists ${listed.length} files, including the bundle and the grammar`);

const result = spawnSync(
  fileURLToPath(vsceCli),
  ["package", "--no-dependencies", "--out", fileURLToPath(output)],
  {cwd: fileURLToPath(new URL("..", import.meta.url)), stdio: "inherit"}
);

if (result.status !== 0) {
  process.exit(result.status ?? 1);
}

await access(output);

// The universal package is the package for every platform. It carries no
// server, because whoever installs it has `jasmin-lsp` from opam or from a
// release asset already, and a binary built for one platform would be the
// wrong binary on every other one.
const universal = readZip(await readFile(output));
const universalServerEntries = universal
  .paths()
  .filter((path) => path.startsWith(`extension/${serverDirectory}/`))
  .sort();

assert.deepEqual(
  universalServerEntries,
  [],
  `the universal package carries ${universalServerEntries.join(", ")}; it must carry no ${serverDirectory}/`
);

assert.ok(
  !universal.text("extension.vsixmanifest").includes("TargetPlatform="),
  "the universal package declares a target platform"
);

console.log(
  `universal package: ${universal.paths().length} entries and no ${serverDirectory}/ directory`
);

// Every platform package in `.artifacts/` is read back. `validate` produces
// only the universal one, so an empty list is the normal case and is reported
// as one; `scripts/package-vsix.mjs` adds a platform package, and this then
// holds it to the contract it is shipped under: the server at the path the
// extension's lookup reads, with mode 0755, and a digest file that names it
// and matches its bytes.
const artifacts = await readdir(artifactsDirectory);
const built = [];

for (const platform of bundledPlatforms) {
  for (const name of artifacts) {
    if (name.startsWith(`jasmin-ide-${platform.vsceTarget}-`) && name.endsWith(".vsix")) {
      built.push({platform, name});
    }
  }
}

if (built.length === 0) {
  console.log(
    "platform packages: none in .artifacts/, so none was checked " +
      "(scripts/package-vsix.mjs <platform> <binary> builds one)"
  );
}

for (const {platform, name} of built) {
  const archive = readZip(await readFile(new URL(name, artifactsDirectory)));
  const expectedServer = `extension/${packagedServerPath(platform)}`;
  const expectedDigest = `extension/${packagedDigestPath(platform)}`;

  const serverEntries = archive
    .paths()
    .filter((path) => path.startsWith(`extension/${serverDirectory}/`))
    .sort();

  assert.deepEqual(
    serverEntries,
    [expectedDigest, expectedServer].sort(),
    `the ${platform.name} package holds ${serverEntries.join(", ") || "nothing"} under ` +
      `${serverDirectory}/, not ${packagedServerPath(platform)} and ${packagedDigestPath(platform)}`
  );

  const mode = archive.mode(expectedServer);
  assert.equal(
    mode & 0o777,
    0o755,
    `the ${platform.name} server is packaged with mode ${(mode & 0o777).toString(8)}, not 0755`
  );

  const entries = parseDigestFile(archive.text(expectedDigest));
  assert.deepEqual(
    [...entries.keys()],
    [serverExecutable],
    `the ${platform.name} digest file names ${[...entries.keys()].join(", ") || "nothing"}`
  );

  assert.equal(
    entries.get(serverExecutable),
    digestOfBytes(archive.read(expectedServer)),
    `the ${platform.name} digest file does not match the server it ships`
  );

  assert.ok(
    archive.text("extension.vsixmanifest").includes(`TargetPlatform="${platform.vsceTarget}"`),
    `the ${platform.name} package does not declare TargetPlatform="${platform.vsceTarget}"`
  );

  console.log(
    `${platform.name} package: ${packagedServerPath(platform)} at mode 0755, ` +
      `${packagedDigestPath(platform)} matches, ${archive.paths().length} entries`
  );
}
