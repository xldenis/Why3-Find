  $ why3find prove foo.mlw -r -P alt-ergo --stdlib -a --stdlib --extern --builtin
  Theory foo.S: ✔ (-)
    Parameter  logic foo.S.op
    Hypothesis axiom foo.S.com
  Theory foo.T: ✔ (2)
  Dependencies:
    External   value int.Int.(=) (stdlib, extern)
    External   param int.Int.(-) (stdlib, builtin, extern)
    External   param int.Int.(+) (stdlib, builtin, extern)
    External   param int.Int.(*) (stdlib, builtin, extern)
    External   value int.Int.(<) (stdlib, builtin, extern)
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
