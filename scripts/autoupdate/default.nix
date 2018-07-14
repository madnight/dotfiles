{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation
      , base
      , stdenv
      , concurrent-extra
      , monad-extras
      , wreq
      , timerep
      , xml
  }:
  mkDerivation {
    pname = "autoupdate";
    version = "1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends =
      [ base concurrent-extra monad-extras wreq timerep xml];
    license = stdenv.lib.licenses.bsd3;
  };

  haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callPackage f {});

in

if pkgs.lib.inNixShell then drv.env else drv
