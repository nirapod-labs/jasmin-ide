let
  flake-compat = builtins.fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/refs/heads/master.tar.gz";
  };
in
(import flake-compat {
  src = ./.;
}).defaultNix
