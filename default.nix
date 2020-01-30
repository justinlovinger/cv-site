# pin nixpkgs version for reproducible builds
{ pkgs ? (import (builtins.fetchGit {
  url = https://github.com/NixOS/nixpkgs-channels.git;
  ref = "nixos-19.09";
  rev = "b926503738cc5b19f3ee66f8a5745c41318a82ff";
}) {}) }:

let
  pname = "justinlovinger-cv-site";
  version = "1.0.0";

  nodejs = pkgs.nodejs;
  # Use PureScript
  # and related packages
  # from [easy-purescript-nix]
  # (https://github.com/justinwoo/easy-purescript-nix).
  # This is updated more frequently than nixpkgs versions.
  easy-ps = import (builtins.fetchGit {
    url = https://github.com/justinwoo/easy-purescript-nix.git;
    ref = "master";
    rev = "01ae1bc844a4eed1af7dfbbb202fdd297e3441b9";
  }) { inherit pkgs; };

  # Create a simple derivation
  # containing a node_modules/ directory.
  localNodePkgs = import ./node-packages.nix {
    inherit pkgs;
    inherit (pkgs.stdenv.hostPlatform) system;
    inherit nodejs;
  };
  # This is a convenience function
  # for symlinking node2nix node packages.
  linkNodePackage = nodePkg: "ln -s ${nodePkg}/lib/node_modules/* $out/lib/node_modules/";
  nodeModules = pkgs.stdenv.mkDerivation {
    pname = "${pname}-node-modules";
    version = version;
    installPhase = ''
      mkdir -p $out/lib/node_modules/
      ${linkNodePackage localNodePkgs.react}
      ${linkNodePackage localNodePkgs.react-dom}
    '';
    phases = [ "installPhase" ];
  };

  # Install spago packages with Nix
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
  # Fix [`spagoPkgs` error]
  # (https://github.com/justinwoo/spago2nix/issues/4#issuecomment-512904817)
  spagoBuildFromNixStore = spagoPkgs.buildFromNixStore.overrideAttrs (oldAttrs: {
    buildCommand = builtins.replaceStrings ["#!/usr/bin/env bash"] ["#!/bin/sh"] oldAttrs.buildCommand;
  });
in pkgs.stdenv.mkDerivation {
  pname = pname;
  version = version;
  src = ./.;
  buildInputs = [
    easy-ps.purescript
    nodejs
    pkgs.closurecompiler
    pkgs.nodePackages.parcel-bundler
  ];
  preConfigurePhases = [ "cleanPhase" ];
  cleanPhase = ''
    rm -rf .cache .spago2nix dist node_modules output result src/Generated
  '';
  configurePhase = ''
    # Generate PureScript files
    mkdir -p src/Generated
    echo "module Generated.Files where" > src/Generated/Files.purs
    echo 'files = ${builtins.replaceStrings ["="] [":"] (builtins.toJSON (import ./files.nix))}' >> src/Generated/Files.purs

    # Install dependencies
    # `parcel` does not support `NODE_PATH`
    # so instead of
    # `export NODE_PATH=${nodeModules}/lib/node_modules`
    # we link to working directory
    rm -rf node_modules && ln -s ${nodeModules}/lib/node_modules ./node_modules
    ${spagoBuildFromNixStore} "src/Main.purs" "src/**/*.purs"
  '';
  buildPhase = ''
    export BUILDDIR=$(mktemp -d -p .)
    purs bundle "output/*/*.js" -m Main --main Main --output $BUILDDIR/index.raw.js
    closure-compiler --js $BUILDDIR/index.raw.js --js_output_file $BUILDDIR/index.js
    cp index.html $BUILDDIR/index.html
    parcel build $BUILDDIR/index.html --no-source-maps
    rm -r $BUILDDIR
  '';
  doCheck = true;
  checkPhase = ''
    ${spagoBuildFromNixStore} "test/Main.purs" "src/**/*.purs" "test/**/*.purs"
    NODE_PATH=output node -e "require('Test.Main').main();"
  '';
  installPhase = ''
    mkdir -p $out

    # Add app
    cp -r dist $out/

    # Add script to host app
    mkdir -p $out/bin
    echo "#!/bin/sh" > $out/bin/concur-landing-page
    echo "${pkgs.nodePackages.serve}/bin/serve \"\$@\" $out/dist/" >> $out/bin/concur-landing-page
    chmod +x $out/bin/concur-landing-page
  '';
  shellHook = ''
    # Add shell binaries
    # No indent in multi-line command
    # because we can't have spaces between path additions
    export PATH=\
    ${pkgs.nodePackages.node2nix}/bin:\
    ${easy-ps.spago2nix}/bin:\
    $PATH

    # Add dependencies
    eval "$configurePhase"
  '';
  meta = with pkgs.stdenv.lib; {
    description = "The personal portfolio website for Justin Lovinger";
    # homepage = ;
    license = licences.mit;
    maintainers = [ ];
  };
}
