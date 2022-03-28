{ system ? builtins.currentSystem
, crossSystem ? null
  # allows to cutomize haskellNix (profiling, see ./nix/shardagnostic-network.nix)
, config ? { }
  # allows to override dependencies of the project without modifications,
  # eg. to test build against local checkout of nixpkgs and tbco-nix:
  # nix build -f default.nix bcc-ledger-cole --arg sourcesOverride '{
  #   tbco-nix = ../tbco-nix;
  # }'
, sourcesOverride ? { }
  # pinned version of nixpkgs augmented with overlays (tbco-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.tbcoNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let
  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages arkNetworkHaskellPackages);

  validate-mainnet = import ./nix/validate-mainnet.nix {
    inherit pkgs;
    cole-db-converter = haskellPackages.shardagnostic-consensus-cole.components.exes.db-converter;
    db-analyser = haskellPackages.shardagnostic-consensus-bcc.components.exes.db-analyser;
    onlyImmutableDB = false;
  };

  self = {
    inherit haskellPackages network-docs consensus-docs;

    inherit (haskellPackages.shardagnostic-network.identifier) version;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    libs = collectComponents' "library" haskellPackages;

    exes = collectComponents' "exes" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks' haskellPackages;
    };

    # These are not run on hydra, but will be built separately in a nightly
    # build job.
    nightly-checks = {
      inherit validate-mainnet;
      gnuparallel = pkgs.parallel;
      glibcLocales = pkgs.glibcLocales;

      Bcc = haskellPackages.shardagnostic-consensus-bcc-test.components.tests.test;
      Sophie = haskellPackages.shardagnostic-consensus-sophie-test.components.tests.test;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in
self
