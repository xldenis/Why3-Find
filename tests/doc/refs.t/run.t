--------------------------------------------------------------------------
--- Testing References
--------------------------------------------------------------------------
  $ why3find doc a.mlw
  $ find html
  html
  html/a.A.html
  html/style.css
  html/a.html
  html/a.B.html
--------------------------------------------------------------------------
--- Library
--------------------------------------------------------------------------
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
  <p>Infix reference: <code class="src"><a href="https://why3.lri.fr/stdlib/int.html#infix +_19">int.Int.(+)</a></code></p>
  <p>Prefix reference: <code class="src"><a href="https://why3.lri.fr/stdlib/int.html#prefix -_18">int.Int.(-_)</a></code></p>
  <p>Value reference: <code class="src"><a href="https://why3.lri.fr/stdlib/int.html#max_65">int.MinMax.max</a></code></p>
  <p>Local references <code class="src"><a href="a.A.html#t_12">A.t</a></code> and <code class="src"><a href="a.A.html#op_13">op</a></code>.</p>
  </div>
  <div class="src theory">theory <tt><a href="a.A.html">A</a></tt></div>
  <div class="src module">module <tt><a href="a.B.html">B</a></tt></div>
  </body>
  </html>
--------------------------------------------------------------------------
--- Theory A
--------------------------------------------------------------------------
  $ cat html/a.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Theory a.A</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Theory <tt><a href="a.html">a</a>.A</tt></header>
  <pre class="src">
  <span class="keyword">theory</span> <a name="A_">A</a>
    <span class="keyword">type</span> <a name="t_12">t</a>
    <span class="keyword">function</span> <a name="op_13">op</a> <a href="#t_12">t</a> <a href="#t_12">t</a> : <a href="#t_12">t</a>
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module B
--------------------------------------------------------------------------
  $ cat html/a.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module a.B</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <tt><a href="a.html">a</a>.B</tt></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="B_">B</a>
    <span class="keyword">type</span> <a name="t_17">t</a>
    <span class="keyword">val</span> <a name="binop_18">binop</a> <a href="#t_17">t</a> <a href="#t_17">t</a> : <a href="#t_17">t</a>
  
  </pre>
  <div class="doc">
  <p>B reference <code class="src"><a href="#t_17">t</a></code></p>
  </div>
  <pre class="src">
  
  </pre>
  <div class="doc">
  <p>A reference <code class="src"><a href="a.A.html#op_13">A.op</a></code></p>
  </div>
  <pre class="src">
  
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
