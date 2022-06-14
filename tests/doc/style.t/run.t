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
  <a class="toc1" href="#_1">Title A</a>
  <a class="toc2" href="#_2">Title B</a>
  <a class="toc2" href="#_3">Title E</a>
  <a class="toc3" href="#_4">Title C</a>
  <a class="toc3" href="#_5">Title D</a>
  <a class="toc2" href="#_6">Title F</a>
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
  <div class="doc">
   Headings.
  
   <h1><a name="_1">Title A</a></h1>
   Section 1
  
   <h2><a name="_2">Title B</a></h2>
   Section 1
  
   <h2><a name="_3">Title E</a></h2>
   Section 2
  
   <h3><a name="_4">Title C</a></h3>
   Section 1
  
   <h3><a name="_5">Title D</a></h3>
   Section 2
  
   <h2><a name="_6">Title F</a></h2>
   Section 3
  </div>
  </body>
  </html>
--------------------------------------------------------------------------
