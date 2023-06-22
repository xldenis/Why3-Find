--------------------------------------------------------------------------
--- Testing Styles
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
  <header>Library <a href="index.html"><code>a</code></a></header>
  <div class="doc">
  <p>Testing Documentation.</p>
  <p>This is <em>emph text</em>.</p>
  <p>This is <strong>bold text</strong>.</p>
  <p>This is <code class="src">verbatim</code> text.</p>
  <p>This is <strong>multiple line bold text</strong>.</p>
  <p>Nested <em>emph <strong>bold</strong> emph</em>.</p>
  </div>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
