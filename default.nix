# pin nixpkgs version for reproducible builds
{ pkgs ? (import (builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs.git";
  ref = "nixos-20.09";
  rev = "06b11191834abae2b9ccace27818b74fe5a4b293";
}) {}) }:

let
  pname = "justinlovinger-cv-site";
  version = "1.0.7";

  nodejs = pkgs.nodejs;

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
      ${linkNodePackage localNodePkgs."normalize.css"}
    '';
    phases = [ "installPhase" ];
  };

  # Install spago packages with Nix
  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
in pkgs.stdenv.mkDerivation {
  pname = pname;
  version = version;
  src = ./.;
  buildInputs = [
    nodejs
    pkgs.closurecompiler
    pkgs.nodePackages.parcel-bundler
    pkgs.purescript
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
    ${spagoPkgs.buildFromNixStore}/bin/build-from-store "src/Main.purs" "src/**/*.purs"
  '';
  buildPhase = ''
    # Create build directory
    export BUILDDIR=$(mktemp -d -p .)

    # Link css to build directory
    mkdir -p $BUILDDIR/node_modules
    ln -s ${localNodePkgs."normalize.css"}/lib/node_modules/* $BUILDDIR/node_modules/
    # Parcel requires cssnano to bundle css
    ln -s ${localNodePkgs.cssnano}/lib/node_modules/* $BUILDDIR/node_modules/

    # Build
    purs bundle "output/*/*.js" -m Main --main Main --output $BUILDDIR/index.raw.js
    closure-compiler --js $BUILDDIR/index.raw.js --js_output_file $BUILDDIR/index.js
    cp index.html $BUILDDIR/index.html
    parcel build $BUILDDIR/index.html --no-source-maps

    # Cleanup build directory
    rm -r $BUILDDIR
  '';
  doCheck = true;
  checkPhase = ''
    ${spagoPkgs.buildFromNixStore}/bin/build-from-store "test/Main.purs" "src/**/*.purs" "test/**/*.purs"
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
    ${import (builtins.fetchGit {
      url = https://github.com/justinwoo/spago2nix.git;
      ref = "master";
      rev = "898798204fa8f53837bbcf71e30aeb425deb0906";
    }) { inherit pkgs nodejs; }}/bin:\
    $PATH

    # Add dependencies
    eval "$configurePhase"
  '';
  meta = with pkgs.stdenv.lib; {
    description = "The personal portfolio, resume, CV, website for Justin Lovinger";
    homepage = https://github.com/JustinLovinger/cv-site;
    license = licenses.mit;
    maintainers = [ ];
  };
}
