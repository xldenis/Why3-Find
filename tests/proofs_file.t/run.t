We don't actually run any proofs but just check if the proof file get generated

  $ why3find prove foo.mlw
  Theory foo.S: ✔ (-)
  Theory foo.T: ✔ (-)

Has proofs.json been correctly generated ?
  $ ls foo/
  proof.json
