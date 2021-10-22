# Adding a new Sophie-based era to consensus

This document walks through the steps required to add a new Sophie-based era to
consensus and to add it as an extra era to the Bcc blockchain.

Prior art upon which this is based:
* [#2666](https://github.com/The-Blockchain-Company/shardagnostic-network/pull/2666)
* [#2679](https://github.com/The-Blockchain-Company/shardagnostic-network/pull/2679)

The steps are fairly straightforward. We have put things in place (in consensus
and in the ledger) when adding the Evie and Jen eras that should now make it
much easier to add a new additional era.

There are two main driving changes: adding the new era to
`Shardagnostic.Consensus.Sophie.Eras` and including that era in
`Shardagnostic.Consensus.Bcc.Block`. The compiler should point the rest of the
way. It's mostly a matter of following the existing patterns by imitating the
previous Sophie-based case. Be sure to run both the Sophie
(`shardagnostic-consensus-sophie-test`) and the Bcc
(`shardagnostic-consensus-bcc-test`) tests.

For exhaustiveness, we give an overview of the changes needed. The new era we'll
be adding is the Aurum era, which comes after the Jen era.

## Preparation

* Locate the new tag in the ledger, e.g., `AurumEra`. This is an empty data
  type that is used at the type level to indicate the era. The ledger should
  have an instance of the `SophieBasedEra` class (the class defined in
  `Sophie.Spec.Ledger.API`, do not confuse it with the class with the same name
  in consensus) for this era. This class should provide all the instances
  consensus integration will rely on.

* Note that both the affected consensus libraries and test suites are parametric
  in the Sophie-based era or in the general era. The test suite relies on the
  necessary `Arbitrary`, `Show`, `Eq`, ... instances to be available for all of
  the Bcc eras. This means that a new era can only be added to the Bcc
  eras when the ledger provides all the necessary instances for the new era,
  *including the instances for the test suite* (e.g., the serialisation
  generators). Note that because the test suite is parametric/generic in the
  eras, it is not easy to temporarily omit a single era from the test suite
  until the required instances have been added. This means that we can't extend
  the library with the new era without extending the test suite too. In the
  past, this has been the main blocker: the library was ready, but the required
  instances in the ledger were missing for our test suite(s).

* Add the required dependencies on the ledger package (and its test package) to
  the `cabal.project` file. You will have to add it to some other cabal files
  too, let the compiler tell you which ones.

## `shardagnostic-consensus-sophie`

* Define `StandardAurum` in `Shardagnostic.Consensus.Sophie.Eras` and add any
  missing instances to that module, update the export list appropriately.

* In `Shardagnostic.Consensus.Sophie.Node`, define `ProtocolParamsAurum` just like
  the existing ones. In case the era in question needs extra parameters (e.g., a
  new genesis config?), they can be added here.

## `shardagnostic-consensus-sophie-test`

* In `Test.Consensus.Sophie.Examples`, define an `examplesAurum` similar to
  how `examplesJen` is defined. If necessary, i.e., when new type families have
  been created in the ledger (e.g., `TxOut`), the `examples` function might have
  to take more arguments. This is where the golden test examples are defined,
  but only the Sophie ones are tested as part of this test suite. The others
  are only tested as part of the `shardagnostic-consensus-bcc-test` test suite.

## `shardagnostic-consensus-bcc`

* In `Shardagnostic.Consensus.Bcc.Block`, include `AurumEra` in `BccEras`
  and `SophieBasedEras`. Update all the pattern synonyms in the module with the
  new era. Don't forget to update the comments, the `COMPLETE` pragmas, and the
  export lists. It's easy to forget a case and the compiler will likely not warn
  you, you'll notice it when trying to use the pattern synonyms.

* In `Shardagnostic.Consensus.Bcc.CanHardFork`, update
  `BccHardForkConstraints`, add additional translations to the `CanHardFork`
  instance.

* In `Shardagnostic.Consensus.Bcc.Node`, update the `SerialiseHFC` instance by
  following the existing patterns. Add a new `BccNodeToNodeVersion` and
  `BccNodeToClientVersion` that enable the `AurumEra`, update the existing
  ones so that they disable the new era. Be sure to include the new versions in
  the two methods of the `SupportedNetworkProtocolVersion` instance. Extend
  `protocolInfoBcc` with the new era by following the type errors and adding
  the missing parameters (including `ProtocolParamsTransition`). Don't forget to
  derive `maxMajorProtVer` from the new final era. Update
  `protocolClientInfoBcc` too.

* In `Shardagnostic.Consensus.Bcc`, update the `ProtocolBcc` type synonym,
  add the extra arguments needed for `protocolInfoBcc` to the
  `ProtocolBcc` constructor. Update `protocolInfo` accordingly.

## `shardagnostic-consensus-bcc-test`

* In `Test.Consensus.Bcc.Generators`, update `arbitraryNodeToNode`,
  `arbitraryNodeToClient`. Try to understand the logic of these two, you will
  also have to add a new case to these. It would be nice if we could write these
  two in a generic way so that they won't have to be updated with each era, but
  it's not so simple. Update the other functions/instances. Be careful, you
  won't get warnings for missing cases in `Arbitrary` instances, so go over all
  of them and add the missing cases.

* In `Test.Consensus.Bcc.ColeCompatibility`, update `toBccCodecConfig`.

* In `Test.Consensus.Bcc.Serialisation`, update `testCodecCfg` and
  `dictNestedHdr`.

* In `Test.Consensus.Bcc.Examples`, update `eraExamples`, `combineEras`, and
  the rest.

* In `Test.Consensus.Bcc.Golden`, update the `ToGoldenDirectory` instances.

* Create a `Test.ThreadNet.JenAurum` module similar to
  `Test.ThreadNet.EvieJen`, to test the transition from Jen to Aurum.
  Don't forget to include it in the `Main` of the test suite. You will have to
  create a `Test.ThreadNet.TxGen.Aurum` module similar to
  `Test.ThreadNet.TxGen.Jen`.

* Run the golden tests of `shardagnostic-consensus-bcc-test`. Golden test
  results should have been created for the new Bcc versions. Don't forget to
  commit those files, otherwise they will be recreated on each run in CI and not
  compared against the previous results, rendering them useless.

* Extend `Test.ThreadNet.Bcc` with the new era. At the time of writing this
  hasn't been done yet for Evie or Jen, though.
