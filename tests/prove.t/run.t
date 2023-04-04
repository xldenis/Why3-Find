  $ why3find prove foo.mlw -r -P alt-ergo --stdlib -h --stdlib --extern --builtin
  Theory foo.S: ✔ (-)
    Parameter  logic foo.S.op
    Hypothesis axiom foo.S.com
    Procedure  value foo.S.com'lemma
  Theory foo.T: ✔ (2)
    Procedure  value foo.T.Add.com'lemma
    Procedure  value foo.T.Mul.com'lemma
  Dependencies:
    External   value int.Int.(=) (stdlib, extern)
    External   value int.Int.(-) (stdlib, builtin, extern)
    External   value int.Int.(+) (stdlib, builtin, extern)
    External   value int.Int.(*) (stdlib, builtin, extern)
    External   value int.Int.(<) (stdlib, builtin, extern)
    Standard   value int.Int.Assoc'lemma (stdlib)
    Standard   value int.Int.Unit_def_l'lemma (stdlib)
    Standard   value int.Int.Unit_def_r'lemma (stdlib)
    Standard   value int.Int.Inv_def_l'lemma (stdlib)
    Standard   value int.Int.Inv_def_r'lemma (stdlib)
    Standard   value int.Int.Comm'lemma (stdlib)
    Standard   value int.Int.MulAssoc.Assoc'lemma (stdlib)
    Standard   value int.Int.Mul_distr_l'lemma (stdlib)
    Standard   value int.Int.Mul_distr_r'lemma (stdlib)
    Standard   value int.Int.MulComm.Comm'lemma (stdlib)
    Standard   value int.Int.Unitary'lemma (stdlib)
    Standard   value int.Int.NonTrivialRing'lemma (stdlib)
    Standard   value int.Int.Refl'lemma (stdlib)
    Standard   value int.Int.Trans'lemma (stdlib)
    Standard   value int.Int.Antisymm'lemma (stdlib)
    Standard   value int.Int.Total'lemma (stdlib)
    Standard   value int.Int.ZeroLessOne'lemma (stdlib)
    Standard   value int.Int.CompatOrderAdd'lemma (stdlib)
    Standard   value int.Int.CompatOrderMult'lemma (stdlib)
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
