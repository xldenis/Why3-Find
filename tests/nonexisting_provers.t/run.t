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
  Error: 1 unproved file(s)
  Warning summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
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
  Error: 1 unproved file(s)
  Warning summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Emitted 5 warnings, 1 error
  [1]
