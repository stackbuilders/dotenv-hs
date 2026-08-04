{ pkgs, ... }:

{
  # https://devenv.sh/reference/options/
  packages = with pkgs; [
    cabal-install
    ghc
    haskell-language-server
    hlint
    ormolu
    stack
    zlib
  ];

  enterShell = ''
    echo "dotenv-hs dev environment loaded"
    echo "Try: cabal build && cabal test"
  '';

  languages.haskell.enable = true;
}
