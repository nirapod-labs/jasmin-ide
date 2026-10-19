{
  description = "Language server for the Jasmin language";

  # Inputs.
  #
  # `nixpkgs` and `flake-parts` are named by a branch below and are pinned
  # by `flake.lock`, not here. A revision written into this file would have
  # to be a real one, and this file was written on a host without `nix`, so
  # no revision could be taken from a `nix flake lock` run. `flake.lock` is
  # therefore not in the tree yet: run
  #
  #   nix flake lock
  #
  # once on a host with `nix` and commit what it writes. Until then a Nix
  # command reads the branch named here, which is a floating tree, and this
  # file is the only place that says so. docs/nix.md records what the lock
  # is for, and what a host with `nix` has to check.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      # The three platforms this project targets. macOS x86-64
      # and Windows are not targets of it.
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        {
          pkgs,
          lib,
          ...
        }:
        let
          ocamlPackages = pkgs.ocamlPackages;

          ##################################################################
          # The Jasmin release, read from the one file that records it.
          #
          # vendor/jasmin.lock holds `key = value` lines, not TOML, so it is
          # read line by line. Reading it here rather than repeating the
          # values means the flake and `vendor/fetch.sh` cannot drift onto
          # two different releases, and that the digest this file verifies
          # is the digest the repository already committed.
          ##################################################################
          lockText = builtins.readFile ./vendor/jasmin.lock;

          # One `key = value` line, read as `vendor/fetch.sh` reads it: the
          # key has to start the line (after optional whitespace) and be
          # followed by `=`, and the value is the rest of the line. A prefix
          # test would answer a `version_date` line when `version` was asked
          # for, and taking the last `=`-separated field would cut a URL that
          # carries a query string. `builtins.match` matches the whole line,
          # so the pattern is anchored at both ends and `(.*)` holds the value.
          lockField =
            name:
            let
              values = builtins.filter (value: value != null) (
                map (line: builtins.match "[[:space:]]*${name}[[:space:]]*=[[:space:]]*(.*)" line) (
                  lib.splitString "\n" lockText
                )
              );
            in
            if values == [ ] then
              throw "vendor/jasmin.lock has no ${name} line"
            else
              lib.trim (builtins.head (builtins.head values));

          version = lockField "version";
          sha256 = lockField "sha256";
          upstream = lockField "upstream";

          ##################################################################
          # Where the Jasmin library comes from.
          #
          # The server links the OCaml library `jasmin.jasmin`, so this file has to
          # say which derivation provides it. Two are possible, and they are
          # not equivalent:
          #
          #   "tarball"  a derivation from the release tarball this
          #              repository already pins in vendor/jasmin.lock. Every
          #              input is in the tree, and the digest is a value the
          #              repository can check.
          #
          #   "nixpkgs"  nixpkgs' own `jasmin-compiler`, taking its `lib`
          #              output, on the assumption that the output exposes
          #              `jasmin.jasmin` to a downstream dune build. Whether
          #              it does is UNKNOWN: no host this file was written on
          #              has `nix`, so the nixpkgs expression could be
          #              neither read nor evaluated. This path is written and
          #              unexercised.
          #
          # The default is "tarball", because it is the one whose inputs this
          # repository pins. A host with `nix` that finds the nixpkgs path
          # works sets this to "nixpkgs" and runs the checks again.
          ##################################################################
          jasminLibrary = "tarball";

          # The release tarball, assembled the way vendor/fetch.sh assembles
          # it: the compiler's `src` and `uint63` trees with their LICENSE,
          # MANIFEST and dune-project, and nothing else. The dune files are
          # the archive's own, with no edit, so the library keeps the public
          # name upstream gives it, `jasmin.jasmin`, which is the name the
          # repository's dune files ask for; the archive's dune-project
          # declares the package `jasmin` that name belongs to. The safety
          # library, apron and PPL are not built.
          #
          # `buildDunePackage` rather than a plain derivation, so the library
          # is installed into the OCaml site-lib directory that the server's
          # build searches; `dune install --prefix` alone would put it under
          # `$out/lib/jasmin`, where findlib does not look.
          jasminTarball = ocamlPackages.buildDunePackage {
            pname = "jasmin";
            inherit version;

            duneVersion = "3";

            src = pkgs.fetchurl {
              url = upstream;
              inherit sha256;
            };

            # Unpack, then keep only the part the library needs. The archive
            # holds exactly one top-level directory,
            # `jasmin-compiler-v<version>/`.
            unpackPhase = ''
              runHook preUnpack
              mkdir source
              tar -xf "$src" -C source --strip-components=1
              mkdir lib
              mv source/compiler/src lib/src
              mv source/compiler/uint63 lib/uint63
              mv source/compiler/LICENSE lib/LICENSE
              mv source/compiler/MANIFEST lib/MANIFEST
              mv source/compiler/dune-project lib/dune-project
              chmod -R u+w lib
              cd lib
              runHook postUnpack
            '';

            nativeBuildInputs = [ ocamlPackages.menhir ];
            propagatedBuildInputs = [
              ocamlPackages.angstrom
              ocamlPackages.batteries
              ocamlPackages.menhirLib
              ocamlPackages.zarith
            ];

            meta = {
              description = "The OCaml library of the Jasmin compiler, for the language server";
              homepage = "https://github.com/jasmin-lang/jasmin";
              license = lib.licenses.mit;
              platforms = lib.platforms.unix;
            };
          };

          # nixpkgs' package, on the assumption written above, which nothing
          # here could check. Only its `lib` output is wanted: the compiler's
          # binaries are not the server's.
          jasminFromNixpkgs = pkgs.jasmin-compiler.lib;

          jasminLibraryPackage =
            if jasminLibrary == "nixpkgs" then
              jasminFromNixpkgs
            else if jasminLibrary == "tarball" then
              jasminTarball
            else
              throw "jasminLibrary must be \"tarball\" or \"nixpkgs\", not \"${jasminLibrary}\"";

          # What the server needs beside the Jasmin library. The names are
          # the dune library names, which `jasmin-lsp.opam` lists under the
          # same names, so the two cannot say different things.
          serverLibraries = [
            jasminLibraryPackage
            ocamlPackages.lsp
            ocamlPackages.jsonrpc
            ocamlPackages.yojson
            ocamlPackages.menhirLib
            ocamlPackages.zarith
          ];

          # The tree as the server builds it: without the vendored library. It
          # is assembled by vendor/fetch.sh, which downloads, so it is not part
          # of a Nix build; the rest of vendor/ is source that no build reads,
          # and vendor/jasmin.lock stays because the opam contract test reads
          # the release it pins. With the tree removed the library comes from
          # `jasminLibraryPackage` above, under the name `jasmin.jasmin` that
          # the released opam package installs as well.
          dropVendor = ''
            rm -rf vendor/jasmin
          '';

          ##################################################################
          # The server.
          ##################################################################
          jasmin-lsp = ocamlPackages.buildDunePackage {
            pname = "jasmin-lsp";
            version = "0.1.0";

            # dune-project asks for dune 3.24 and the lock file pins 3.24.2.
            duneVersion = "3";

            src = ./.;
            postPatch = dropVendor;

            nativeBuildInputs = [ ocamlPackages.menhir ];
            propagatedBuildInputs = serverLibraries;

            # `@install` is the binary and the two documents. `doCheck` would
            # add `@runtest`, and several cram suites need a corpus that is
            # not in the tree; the `tests` check below runs the suites that
            # need none.
            doCheck = false;

            meta = {
              description = "Language server for the Jasmin language";
              homepage = "https://github.com/jasmin-lang/jasmin";
              license = lib.licenses.mit;
              mainProgram = "jasmin-lsp";
              platforms = lib.platforms.unix;
            };
          };

          # The suites that need neither a corpus nor a network, whichever
          # directory they sit in: the library and server units, the end-to-end
          # checks that drive the built server, the test client, and the opam
          # contract. `fuzz/` and `test/mutation/` are left out because they
          # run for minutes on generated inputs, and `test/context/` because its
          # stanza is switched off without a corpus.
          unitTestDirs = [
            "lang/test"
            "jasmin/test"
            "engine/test"
            "lsp/test"
            "controller/test"
            "lsp-server/native/test"
            "test/client/test"
            "test/opam"
          ];
        in
        {
          packages = {
            inherit jasmin-lsp;
            default = jasmin-lsp;

            # The Jasmin library the server links, as a package of its own,
            # so `nix build .#jasmin-library` shows which path a host chose.
            jasmin-library = jasminLibraryPackage;
          };

          # A shell with what the steps of `make ci` need: the libraries the
          # server links, the OCaml toolchain and its formatter, Node and pnpm
          # for the VS Code extension, the Rust toolchain for the Zed
          # extension, the tree-sitter CLI for the grammar, and opam for the
          # lock file.
          #
          # rustup rather than pkgs.rustc, because editor/zed/rust-toolchain.toml
          # pins the channel and the `wasm32-wasip2` target, and rustup is
          # what reads that file.
          devShells.default = pkgs.mkShell {
            inputsFrom = [ jasmin-lsp ];

            # `inputsFrom` folds in the package's native and build inputs, not
            # the ones it propagates, so the OCaml libraries the server links are
            # named again here: a `nix develop` shell has to be able to build the
            # same tree `nix build` builds, and without them it gets menhir and
            # no `lsp`, `jsonrpc`, `yojson`, `zarith` or `menhirLib`.
            packages = serverLibraries ++ [
              ocamlPackages.ocaml
              ocamlPackages.dune_3
              ocamlPackages.ocamlformat
              ocamlPackages.menhir
              # `make deps` and `opam lock` read the opam files.
              pkgs.opam
              # The VS Code extension; pnpm is pinned by `packageManager`.
              pkgs.nodejs_22
              pkgs.pnpm
              # The Zed extension, through the toolchain file.
              pkgs.rustup
              # The grammar's generator.
              pkgs.tree-sitter
              # What vendor/fetch.sh and the shell tests call.
              pkgs.curl
              pkgs.cacert
            ];
          };

          # What `nix flake check` builds. Every attribute here has to
          # succeed for the flake to pass.
          checks = {
            inherit jasmin-lsp;

            # The binary links and starts, and its batch mode prints its
            # usage. This is the smoke test of the installed package, not of
            # the build tree.
            jasmin-lsp-runs =
              pkgs.runCommand "jasmin-lsp-runs-${jasmin-lsp.version}" { } ''
                mkdir -p $out
                ${jasmin-lsp}/bin/jasmin-lsp check --help > $out/usage.txt
                grep -q 'usage: jasmin-lsp check' $out/usage.txt
              '';

            # The suites that take no corpus. The build phase builds them and
            # the check phase runs them, so a failing assertion fails the
            # flake rather than a step someone has to remember.
            tests =
              ocamlPackages.buildDunePackage {
                pname = "jasmin-lsp-tests";
                inherit (jasmin-lsp) version;

                duneVersion = "3";
                src = ./.;
                postPatch = dropVendor;

                nativeBuildInputs = [ ocamlPackages.menhir ];
                propagatedBuildInputs = serverLibraries;

                # The test executables only: `-p` would ask for a package's
                # install description and this derivation installs nothing
                # but its own success marker.
                buildPhase = ''
                  runHook preBuild
                  dune build -j"$NIX_BUILD_CORES" ${lib.concatStringsSep " " unitTestDirs}
                  runHook postBuild
                '';

                doCheck = true;
                checkPhase = ''
                  runHook preCheck
                  dune runtest -j"$NIX_BUILD_CORES" ${lib.concatStringsSep " " unitTestDirs}
                  runHook postCheck
                '';

                installPhase = ''
                  runHook preInstall
                  mkdir -p $out
                  echo "unit suites passed on ${pkgs.stdenv.hostPlatform.system}" > $out/result.txt
                  runHook postInstall
                '';
              };
          };

          # `nix run` starts the server on stdio, which is what an editor
          # does with it.
          apps.default = {
            type = "app";
            program = "${jasmin-lsp}/bin/jasmin-lsp";
          };
        };
    };
}
