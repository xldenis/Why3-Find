  $ why3find prove -r foo.mlw
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  "foo.mlw", line 3, characters 8-11: proof failed
  Goal Add.com: ✘
    Add.com
  "foo.mlw", line 3, characters 8-11: proof failed
  Goal Mul.com: ✘
    Mul.com
  Theory foo.S: ✔ (-)
  Theory foo.T: ✘
  Error: 1 unproved file
  Summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Error: 1 unproved file
  Emitted 2 warnings, 1 error
  [1]

  $ why3find prove -r bar.mlw
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  "bar.mlw", line 3, characters 8-11: proof failed
  Goal Add.com: ✘
    Add.com
  Warning: prover alt-ergo@2.4.2 not configured (project)
  "bar.mlw", line 3, characters 8-11: proof failed
  Goal Mul.com: ✘
    Mul.com
  Theory bar.S: ✔ (-)
  Theory bar.T: ✘
  Error: 1 unproved file
  Summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Error: 1 unproved file
  Emitted 5 warnings, 1 error
  [1]

  $ why3find config --check
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Configuration:
   - runner: 2 jobs, 1.0s
   - tactics: split_vc (depth 4)
  Summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Emitted 5 warnings
  [1]
