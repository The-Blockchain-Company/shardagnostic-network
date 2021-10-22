# shardagnostic-consensus-sophie-test

The reason for this being a separate package from
`shardagnostic-consensus-sophie` and not just its testsuite, is that
`shardagnostic-consensus-bcc` depends on the test code of Sophie to test the
Bcc chain, of which Sophie is a part.

This package contains:

* `src`: test generators and test infrastructure for testing the integration
  of the Sophie ledger with the consensus layer. Not the actual tests.

* `test`: the actual Sophie tests
