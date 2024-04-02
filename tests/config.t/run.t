  $ WHY3CONFIG=why3.conf why3find config -P alt-ergo -d 2 \
  >   -T split_vc,inline_goal
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
  $ cat why3find.json
  {
    "configs": [],
    "depth": 2,
    "drivers": [],
    "packages": [],
    "provers": [ "alt-ergo" ],
    "tactics": [ "split_vc", "inline_goal" ],
    "time": 1.0
  }

  $ WHY3CONFIG=why3.conf why3find config --default
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2), z3(4.8.15)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
