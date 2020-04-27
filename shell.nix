{ packageSet ? import ./default.nix { rev = "in-nix-shell"; }
}:
with packageSet; haskell.packages.shellFor {
  nativeBuildInputs = [
    # From nixpkgs
    pkgs.ghcid
    pkgs.git
    pkgs.cacert
    pkgs.nodejs
    pkgs.yarn
    pkgs.zlib
    pkgs.z3
    # Broken on 20.03, needs a backport
    # pkgs.sqlite-analyzer
    pkgs.sqlite-interactive
    # Take cabal from nixpkgs for now, see below
    pkgs.cabal-install

    # Deployment tools
    pkgs.terraform_0_11
    pkgs.awscli
    pkgs.aws_shell

    # Extra dev packages acquired from elsewhere
    # FIXME: Can't use this cabal until https://github.com/input-output-hk/haskell.nix/issues/422 is fixed
    #dev.packages.cabal-install
    dev.packages.hlint
    dev.packages.stylish-haskell
    dev.packages.purty
    dev.packages.purs
    dev.packages.spago
    pkgs.stack
  ];
}
