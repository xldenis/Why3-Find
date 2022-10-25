  $ export BUILD_PATH_PREFIX_MAP="WHERE=`why3find where`:$BUILD_PATH_PREFIX_MAP"
  $ why3find prove -P alt-ergo a
  Theory a.foo.Neutral: ✔ (1)
  $ why3find doc a
  Generated $TESTCASE_ROOT/html
  $ why3find install a --global
  install (source)   a/foo.mlw
  install (proof)    a/foo/proof.json
  install (html)     html/a.foo.Neutral.html
  install (html)     html/a.foo.index.html
  install (html)     html/a.foo.proof.html
  install (html)     html/fonts/icofont.woff
  install (html)     html/fonts/icofont.woff2
  install (html)     html/icofont.min.css
  install (html)     html/script.js
  install (html)     html/style.css
  install (meta)     META.json
  Installed WHERE/a
  $ rm -fr a
  $ why3find list
  WHERE/a
  $ why3find query a
  Package a:
    path: WHERE/a
  $ find `why3find query a -p` | sort
  WHERE/a
  WHERE/a/META.json
  WHERE/a/a
  WHERE/a/a/foo
  WHERE/a/a/foo.mlw
  WHERE/a/a/foo/proof.json
  WHERE/a/html
  WHERE/a/html/a.foo.Neutral.html
  WHERE/a/html/a.foo.index.html
  WHERE/a/html/a.foo.proof.html
  WHERE/a/html/fonts
  WHERE/a/html/fonts/icofont.woff
  WHERE/a/html/fonts/icofont.woff2
  WHERE/a/html/icofont.min.css
  WHERE/a/html/script.js
  WHERE/a/html/style.css
  $ why3find compile -p a b/bar.mlw
  $ why3find config --root . -p a -P alt-ergo -s
  Configuration saved to $TESTCASE_ROOT/why3find.json
  $ why3find prove b
  Theory b.bar.Add: ✔ (1)
  Theory b.bar.Mul: ✔ (1)
  $ why3find doc b
  Generated $TESTCASE_ROOT/html
  $ why3find install --global b > /dev/null
  $ why3find list
  WHERE/a
  WHERE/b
  $ why3find query b
  Package b:
    path: WHERE/b
    depends: a
  $ why3find uninstall b
  remove WHERE/b
  $ why3find list
  WHERE/a
  $ why3find install --global b > /dev/null
  $ why3find uninstall a
  remove WHERE/a
  $ why3find list
  WHERE/b
  $ why3find uninstall a b
  Warning: package a not found
  remove WHERE/b
