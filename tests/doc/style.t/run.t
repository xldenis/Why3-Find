--------------------------------------------------------------------------
--- Testing Styles
--------------------------------------------------------------------------
  $ why3find doc a.mlw
  $ cat html/a.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Library a</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Library <tt>a</tt></header>
  <div class="doc">
  <p>Testing Documentation.</p>
  <p>This is <em>emph text</em>.</p>
  <p>This is <strong>bold text</strong>.</p>
  <p>This is <strong>multiple line bold text</strong>.</p>
  <p>Nested <em>emph <strong>bold</strong> emph</em>.</p>
  </div>
  </body>
  </html>
--------------------------------------------------------------------------
