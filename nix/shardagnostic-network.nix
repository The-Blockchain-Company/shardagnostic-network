############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? { }
  # Enable profiling
, profiling ? config.haskellNix.profiling or false
, libsodium ? pkgs.libsodium
}:
let
  compiler-nix-name = pkgs.localConfig.ghcVersion;
  src = haskell-nix.haskellLib.cleanGit {
    name = "shardagnostic-network-src";
    src = ../.;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject {
      inherit compiler-nix-name src;
    }));

  # This creates the Haskell package set.
  # https://The-Blockchain-Company.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    inherit compiler-nix-name src;
    modules = [

      {
        # Compile all local packages with -Werror:
        packages = lib.genAttrs projectPackages
          (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      }
      {
        # Apply profiling arg to all library components in the build:
        enableLibraryProfiling = profiling;

        # Command-line options for test suites:
        packages.shardagnostic-consensus-bcc-test.components.tests.test.testFlags = lib.mkForce [ "--no-create" ];
      }

      # Options specific to the windows cross-compiled build:
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        # Allow reinstallation of Win32
        nonReinstallablePkgs =
          [
            "rts"
            "ghc-heap"
            "ghc-prim"
            "integer-gmp"
            "integer-simple"
            "base"
            "deepseq"
            "array"
            "ghc-boot-th"
            "pretty"
            "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim"
            "ghcjs-th"
            "ghc-boot"
            "ghc"
            "array"
            "binary"
            "bytestring"
            "containers"
            "filepath"
            "ghc-boot"
            "ghc-compact"
            "ghc-prim"
            # "ghci" "haskeline"
            "hpc"
            "mtl"
            "parsec"
            "text"
            "transformers"
            "xhtml"
            # "stm" "terminfo"
          ];
        # ruby/perl dependencies cannot be cross-built for cddl tests:
        packages.shardagnostic-network.flags.cddl = false;

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];

        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [ ];
        packages.terminal-size.components.library.build-tools = lib.mkForce [ ];
        packages.network.components.library.build-tools = lib.mkForce [ ];

        # Make sure that libsodium DLLs are available for tests
        packages.shardagnostic-consensus-cole-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.shardagnostic-consensus-bcc-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.shardagnostic-consensus-mock-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.shardagnostic-consensus-sophie-test.components.tests.test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.shardagnostic-consensus-test.components.tests.test-consensus.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.shardagnostic-consensus-test.components.tests.test-infra.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.shardagnostic-consensus-test.components.tests.test-storage.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
      })
      # Options for when not compiling to windows:
      ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
        packages.shardagnostic-network.flags.cddl = true;
        packages.shardagnostic-network.components.tests.cddl.build-tools = [ pkgs.cddl pkgs.cbor-diag ];
        packages.shardagnostic-network.components.tests.cddl.preCheck = "export HOME=`pwd`";
      })
    ];
  };
in
pkgSet
