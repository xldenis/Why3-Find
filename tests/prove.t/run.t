  $ why3find prove foo.mlw --local --root . -r
  Entering directory '$TESTCASE_ROOT'
  Theory foo.S: ✔ (-)
  Theory foo.T: ✔ (2)
  $ cat foo/proof.json
  {
    "profile": [ { "prover": "Alt-Ergo,2.2.0", "size": 16, "time": 0.1852 } ],
    "proofs": {
      "S": {},
      "T": {
        "Add.com": { "prover": "alt-ergo", "time": 0.0005 },
        "Mul.com": { "prover": "alt-ergo", "time": 0.0005 }
      }
    }
  }
