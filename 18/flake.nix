{
  description = "18";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/e065200fc90175a8f6e50e76ef10a48786126e1c";

  outputs = { self, nixpkgs }:
    let pkgs = import nixpkgs { system = "x86_64-linux"; };
    in
    {
        defaultPackage.x86_64-linux = pkgs.haskellPackages.developPackage {
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [ cabal-install
                ghcid
                # haskell-language-server
              ]);
          returnShellEnv = true; # required for `nix develop`
        };
  };
}
