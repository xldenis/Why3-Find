  $ why3find prove foo.mlw -r
  Theory foo.S: ✔ (-)
  Theory foo.T: ✔ (2)
  $ cat foo/proof.json
  {
    "profile": [ { "prover": "Alt-Ergo,2.2.0", "size": 16, "time": 0.1852 } ],
    "proofs": {
      "S": {},
      "T": {
        "Add.com": { "prover": "alt-ergo", "time": 0.0004 },
        "Mul.com": { "prover": "alt-ergo", "time": 0.0004 }
      }
    }
  }
