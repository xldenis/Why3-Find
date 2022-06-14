--------------------------------------------------------------------------
--- Testing Styles
--------------------------------------------------------------------------
  $ why3find doc p/a.mlw
  $ cat html/p.a.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Library p.a</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Library <tt>p.a</tt></header>
  <div class="doc">
   Testing Documentation.
  
   This is <em>emph text</em>.
  
   This is <strong>bold text</strong>.
  
   This is <strong>multiple
   line bold text</strong>.
  
   Nested <em>emph </em><strong>bold</strong> emph<em>.
  </div>
  </body>
  </html>
--------------------------------------------------------------------------
