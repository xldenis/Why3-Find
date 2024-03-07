  $ WHY3CONFIG=why3.conf why3find config -P alt-ergo -d 2 \
  >   -T split_vc,inline_goal
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.2.0)
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
