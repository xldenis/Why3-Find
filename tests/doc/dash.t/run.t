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
   List &amp; Dashes
  
   Text with normal&ndash;size dash.
  
   Text with long&mdash;size dash.
  
   &ndash; item (a)
   &ndash; item (b)
   &ndash; sub&ndash;item (b.1)
   &ndash; sub&ndash;item (b.2)
   &ndash; item (c)
  
  </div>
  </body>
  </html>
--------------------------------------------------------------------------
