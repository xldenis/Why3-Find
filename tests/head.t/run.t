--------------------------------------------------------------------------
--- Testing Headings
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
  <nav>
  <a class="toc1" href="#_1">Title A</a>
  <a class="toc2" href="#_2">Title B</a>
  <a class="toc2" href="#_3">Title C</a>
  <a class="toc3" href="#_4">Title D</a>
  <a class="toc3" href="#_5">Title E</a>
  <a class="toc2" href="#_6">Title F</a>
  </nav>
  <header>Library <a href="index.html"><code>a</code></a></header>
  <div class="doc">
  <p>Testing Headings.</p>
  <h1><a id="_1">Title A</a></h1>
  <p>Section 1</p>
  <h2><a id="_2">Title B</a></h2>
  <p>Section 1</p>
  <h2><a id="_3">Title C</a></h2>
  <p>Section 2</p>
  <h3><a id="_4">Title D</a></h3>
  <p>Section 1</p>
  <h3><a id="_5">Title E</a></h3>
  <p>Section 2</p>
  <h2><a id="_6">Title F</a></h2>
  <p>Section 3</p>
  </div>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
