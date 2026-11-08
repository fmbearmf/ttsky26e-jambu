{
  description = "An implementation of TinyJAMBU-128 in Clash";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixpkgs-unstable/nixexprs.tar.zst";
  };

  outputs = inputs:
  let
    inherit (inputs) self;

    forSystems = f:
      builtins.mapAttrs
        (system: pkgs:
          let
            attrs = f system pkgs;
            finalAttrs =
            if builtins.isFunction attrs
            then attrs finalAttrs
            else attrs;
          in
          finalAttrs)
        inputs.nixpkgs.legacyPackages;
  in
  {
    devShells = forSystems (system: pkgs:
      let
        inherit (pkgs) haskellPackages python3 mkShell;

        clashEnv = haskellPackages.ghcWithPackages (hp: [
          hp.clash-ghc
          hp.clash-prelude
        ]);

        pythonEnv = python3.withPackages (ps: [
          ps.cocotb
          ps.pytest
        ]);
      in
      {
        default = mkShell {
          packages = [
            clashEnv
            haskellPackages.haskell-language-server
            pkgs.iverilog
            pkgs.gtkwave
            pkgs.gnumake
            pythonEnv
          ];

          shellHook = ''
            export CLASH_HISTDIR="$PWD/.clash"
          '';
        };
      }
    );

    checks = forSystems (system: pkgs:
      let
        inherit (pkgs) lib stdenv python3;

        pythonEnv = python3.withPackages (ps: [
          ps.cocotb
          ps.pytest
        ]);

        testSource = lib.fileset.toSource {
          root = ./.;
          fileset = ./test;
        };

        artifact = self.packages.${system}.verilog;
      in
      {
        cocotb = stdenv.mkDerivation {
          name = "tt-cocotb-check";
          src = testSource;

          nativeBuildInputs = [
            pkgs.iverilog
            pkgs.gnumake
            pythonEnv
          ];

          buildPhase = ''
            runHook preBuild

            mkdir -p src/verilog/
            ln -sv "${artifact}/Project.topEntity" src/verilog/Project.topEntity

            cd test
            make SRC_DIR="$PWD/../src"

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out
            cp results.xml $out/ || touch $out/passed

            runHook postInstall
          '';
        };
      }
    );

    packages = forSystems (system: pkgs: finalAttrs:
      let
        inherit (pkgs) lib stdenv haskellPackages;

        clashEnv = haskellPackages.ghcWithPackages (hp: [
          hp.clash-ghc
          hp.clash-prelude
        ]);

        haskellSource = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.fileFilter
            (file: file.hasExt "hs" || file.hasExt "lhs")
            ./src;
        };
      in
      {
        default = finalAttrs.verilog;

        verilog = stdenv.mkDerivation {
          pname = "tt-verilog";
          version = "0.0.1";
          src = haskellSource;

          nativeBuildInputs = [ clashEnv ];

          buildPhase = ''
            runHook preBuild

            mkdir -p build $out

            clash \
              -isrc \
              -odir build \
              -hidir build \
              -fclash-hdldir $out \
              --verilog \
              src/Project.hs

            runHook postBuild
          '';

          dontInstall = true;
        };
      });
  };
}
