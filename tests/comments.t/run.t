--------------------------------------------------------------------------
--- Testing Comments
--------------------------------------------------------------------------
  $ why3find doc a.mlw
  Generated $TESTCASE_ROOT/html/index.html
  $ cat html/a.index.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library a</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library a</code></header>
  <span class="comment">(* Skipped Comment (1) *)</span><pre class="src"><span class="keyword">module</span> <a title="a.Foo" href="a.Foo.html">Foo</a><a href="a.proof.html#Foo" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  $ cat html/a.Foo.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module a.Foo</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="a.index.html">a</a></code> — <code>module Foo</code></header>
  <span class="comment">(* Skipped Comment (2) *)</span><pre class="src">
  <span class="keyword">module</span> Foo<a href="a.proof.html#Foo" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="comment">(* Printed
       Huge
       Comment *)</span>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
