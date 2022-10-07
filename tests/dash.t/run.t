--------------------------------------------------------------------------
--- Testing Styles
--------------------------------------------------------------------------
  $ why3find doc a.mlw
  $ cat html/a.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library a</title>
  </head>
  <body>
  <header>Library <code>a</code></header>
  <div class="doc">
  <p>List &amp; Dashes</p>
  <p>Text with normal&ndash;size dash.</p>
  <p>Text with long&mdash;size dash.</p>
  <ul>
  <li>item (a)</li>
  <ul>
  <li>sub&ndash;item (a.1)</li>
  <li>sub&ndash;item (a.2)</li>
  </ul>
  <li>item (b)</li>
  <ul>
  <li>sub&ndash;item (b.1)</li>
  <li>sub&ndash;item (b.2)</li>
  <ul>
  <li>sub&ndash;sub&ndash;item !</li>
  </ul>
  </ul>
  <li>item (c)</li>
  </ul>
  </div>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
