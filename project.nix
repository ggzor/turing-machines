let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; };

  compilerVersion = "ghc8104";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";

  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          turing-machine = super.callCabal2nix "turing-machine" (gitIgnore [./.gitignore] ./.) {};
        };
      };
    };
  };
in {
  inherit pkgs;
  shell = compilerSet.shellFor {
    packages = p: [p.turing-machine];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
      haskellPackages.fourmolu
      haskell-language-server
    ];
  };
  patchelf = compilerSet.shellFor {
    packages = p: [];
    buildInputs = [ pkgs.patchelf ];
  };
}
