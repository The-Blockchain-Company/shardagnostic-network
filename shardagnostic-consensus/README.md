# Consensus

If you're beginning to contribute to the Consensus Layer, start with
[Contributing.md][consensus-contributing].

This package contains:

* `src`: the implementation of the Shardagnostic consensus protocols and required
  components, e.g., the storage layer, mempool, protocol clients and servers,
  etc. This library abstracts over the ledger.

* `docs`: documentation, in particular, `docs/report` contains the technical
  report about the consensus and storage layer.

Related packages:

* `../shardagnostic-consensus-cole`: integration with the Cole ledger, including
  protocol tests simulating various node setups.

* `../shardagnostic-consensus-colespec`: integration with the Cole spec ledger.
  This is used to run the Cole protocol tests in lockstep with the spec to
  detect any discrepancies.

* `../shardagnostic-consensus-sophie`: integration with the Sophie ledger,
  including protocol tests simulating various node setups.

* `../shardagnostic-consensus-bcc`: the consensus instantiated to the ledgers
  the `bcc-node` currently supports.

[consensus-contributing]: docs/Contributing.md
