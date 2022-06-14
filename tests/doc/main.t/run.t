--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc p/a.mlw
--------------------------------------------------------------------------
--- Output Directory
--------------------------------------------------------------------------
  $ find html | sort
  html
  html/p.a.A.html
  html/p.a.Assoc.html
  html/p.a.html
  html/style.css
--------------------------------------------------------------------------
--- Generated File p.a.html
--------------------------------------------------------------------------
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
  <div class="doc">This is normal text.</div>
  <div class="src module">module <tt><a href="p.a.A.html">A</a></tt></div>
  <div class="doc">Normal text continued.</div>
  <div class="src module">module <tt><a href="p.a.Assoc.html">Assoc</a></tt></div>
  </body>
  </html>
--------------------------------------------------------------------------
--- Generated File p.a.Assoc.html
--------------------------------------------------------------------------
  $ cat html/p.a.Assoc.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module p.a.Assoc</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <tt><a href="p.a.html">p.a</a>.Assoc</tt></header>
  <div class="doc">Introducing Assoc.</div>
  <pre class="src">
  <span class="keyword">module</span> <a name="Assoc_">Assoc</a>
    <span class="keyword">use</span> int.<a href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">let</span> <span class="keyword">constant</span> <a name="one_26">one</a> = 1
    <span class="keyword">let</span> <span class="keyword">function</span> <a name="mul_27">mul</a> (a b : int) : int = a <a href="https://why3.lri.fr/stdlib/int.html#infix *_20">*</a> b
    <span class="keyword">clone</span> <a href="#A_">A</a> <span class="keyword">with</span> <span class="keyword">type</span> <a href="#t_8">t</a> = int, <span class="keyword">val</span> <a href="#neutral_9">neutral</a> = <a href="#one_26">one</a>, <span class="keyword">val</span> <a href="#op_10">op</a> = <a href="#mul_27">mul</a>
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- Generated File p.a.A.html
--------------------------------------------------------------------------
  $ cat html/p.a.Assoc.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module p.a.Assoc</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <tt><a href="p.a.html">p.a</a>.Assoc</tt></header>
  <div class="doc">Introducing Assoc.</div>
  <pre class="src">
  <span class="keyword">module</span> <a name="Assoc_">Assoc</a>
    <span class="keyword">use</span> int.<a href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">let</span> <span class="keyword">constant</span> <a name="one_26">one</a> = 1
    <span class="keyword">let</span> <span class="keyword">function</span> <a name="mul_27">mul</a> (a b : int) : int = a <a href="https://why3.lri.fr/stdlib/int.html#infix *_20">*</a> b
    <span class="keyword">clone</span> <a href="#A_">A</a> <span class="keyword">with</span> <span class="keyword">type</span> <a href="#t_8">t</a> = int, <span class="keyword">val</span> <a href="#neutral_9">neutral</a> = <a href="#one_26">one</a>, <span class="keyword">val</span> <a href="#op_10">op</a> = <a href="#mul_27">mul</a>
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- End of Test
--------------------------------------------------------------------------
