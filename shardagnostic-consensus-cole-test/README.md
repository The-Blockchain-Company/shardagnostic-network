# shardagnostic-consensus-cole-test

The reason for this being a separate package from `shardagnostic-consensus-cole`
and not just its testsuite, is that `shardagnostic-consensus-bcc` depends on
the test code of Cole to test the Bcc chain, of which Cole is a part.

This package contains:

* `src`: test generators and test infrastructure for testing the integration
  of the Cole ledger with the consensus layer. It also contains the
  integration of the Cole ledger paired with the Cole spec ledger. Not the
  actual tests.

* `test`: Cole ledger tests, protocol tests simulating various node setups,
  both running (just) the Cole ledger, and the Cole ledger in lockstep with
  the Cole spec ledger to detect any discrepancies.
