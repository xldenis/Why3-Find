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
  <a class="toc1" href="#_1">Title A</a>
  <a class="toc2" href="#_2">Title B</a>
  <a class="toc2" href="#_3">Title E</a>
  <a class="toc3" href="#_4">Title C</a>
  <a class="toc3" href="#_5">Title D</a>
  <a class="toc2" href="#_6">Title F</a>
  </nav>
  <header>Library <code>a</code></header>
  <div class="doc">
  <p>Headings.</p>
  <h1><a name="_1">Title A</a></h1>
  <p>Section 1</p>
  <h2><a name="_2">Title B</a></h2>
  <p>Section 1</p>
  <h2><a name="_3">Title E</a></h2>
  <p>Section 2</p>
  <h3><a name="_4">Title C</a></h3>
  <p>Section 1</p>
  <h3><a name="_5">Title D</a></h3>
  <p>Section 2</p>
  <h2><a name="_6">Title F</a></h2>
  <p>Section 3</p>
  </div>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
