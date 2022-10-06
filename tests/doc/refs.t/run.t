--------------------------------------------------------------------------
--- Testing References
--------------------------------------------------------------------------
  $ why3find doc a.mlw
  $ find html | sort
  html
  html/_a.html
  html/a.A.html
  html/a.B.html
  html/a.html
  html/fonts
  html/fonts/icofont.woff
  html/fonts/icofont.woff2
  html/icofont.min.css
  html/script.js
  html/style.css
--------------------------------------------------------------------------
--- Library
--------------------------------------------------------------------------
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
  <p>Infix reference: <code class="src"><a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20+_19">int.Int.(+)</a></code></p>
  <p>Prefix reference: <code class="src"><a title="int.Int.(-)" href="https://why3.lri.fr/stdlib/int.Int.html#prefix%20-_18">int.Int.(-_)</a></code></p>
  <p>Value reference: <code class="src"><a title="int.MinMax.max" href="https://why3.lri.fr/stdlib/int.MinMax.html#max_65">int.MinMax.max</a></code></p>
  <p>Logic reference: <code class="src"><a title="int.MinMax.max" href="https://why3.lri.fr/stdlib/int.MinMax.html#max_148">int.MinMax.max</a></code></p>
  <p>Local references <code class="src"><a title="a.A.t" href="a.A.html#t">A.t</a></code>, <code class="src"><a title="a.B.binop" href="a.B.html#binop">binop</a></code></p>
  </div>
  <pre class="src"><span class="keyword">theory</span> <a title="a.A" href="a.A.html">A</a><a href="_a.html#a" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="a.B" href="a.B.html">B</a><a href="_a.html#a" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Theory A
--------------------------------------------------------------------------
  $ cat html/a.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Theory a.A</title>
  </head>
  <body>
  <header>Theory <code class="src"><a href="a.html">a</a>.A</code></header>
  <pre class="src">
  <span class="keyword">theory</span> <a id="">A</a><span title="Valid (no goals)" class="icon valid icofont-check"></span>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
    <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="a.A.t" href="#t">t</a> <a title="a.A.t" href="#t">t</a> : <a title="a.A.t" href="#t">t</a>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module B
--------------------------------------------------------------------------
  $ cat html/a.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module a.B</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="a.html">a</a>.B</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a id="">B</a><span title="Valid (no goals)" class="icon valid icofont-check"></span>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
    <span class="keyword">val</span> <a id="binop">binop</a><span title="Constrained Parameter" class="icon remark icofont-question-circle"></span> <a title="a.B.t" href="#t">t</a> <a title="a.B.t" href="#t">t</a> : <a title="a.B.t" href="#t">t</a>
  
  </pre>
  <div class="doc">
  <p>B reference <code class="src"><a title="a.B.t" href="a.B.html#t">t</a></code></p>
  </div>
  <pre class="src">
  
  </pre>
  <div class="doc">
  <p>A reference <code class="src"><a title="a.A.op" href="a.A.html#op">A.op</a></code></p>
  </div>
  <pre class="src">
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
