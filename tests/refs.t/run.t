--------------------------------------------------------------------------
--- Testing References
--------------------------------------------------------------------------
  $ why3find doc main.md a.mlw
  Generated $TESTCASE_ROOT/html/index.html
  $ find html | sort
  html
  html/a.A.html
  html/a.B.html
  html/a.index.html
  html/a.proof.html
  html/fonts
  html/fonts/icofont.woff
  html/fonts/icofont.woff2
  html/icofont.min.css
  html/index.html
  html/main.html
  html/script.js
  html/style.css
--------------------------------------------------------------------------
--- Document
--------------------------------------------------------------------------
  $ cat html/main.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Main</title>
  </head>
  <body>
  <nav>
  <a class="toc1" href="#_1">Main Document</a>
  </nav>
  <header>Main</header>
  <div class="doc">
  <h1><a id="_1">Main Document</a></h1>
  <p>This package contains one [unique](a.index.html) library, which consists of:</p>
  <ul>
  <li>Theory <code class="src"><a title="a.A" href="a.A.html#">a.A</a></code></li>
  <li>Module <code class="src"><a title="a.B" href="a.B.html#">a.B</a></code></li>
  </ul>
  <p>Please, read the doc!</p>
  </div>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Library
--------------------------------------------------------------------------
  $ cat html/a.index.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library a</title>
  </head>
  <body>
  <header>Library <a href="index.html"><code>a</code></a></header>
  <div class="doc">
  <p>Infix reference: <code class="src"><a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix%20+_19">int.Int.(+)</a></code></p>
  <p>Prefix reference: <code class="src"><a title="int.Int.(-)" href="https://why3.lri.fr/stdlib/int.html#prefix%20-_18">int.Int.(-_)</a></code></p>
  <p>Value reference: <code class="src"><a title="int.MinMax.max" href="https://why3.lri.fr/stdlib/int.html#max_67">int.MinMax.max</a></code></p>
  <p>Logic reference: <code class="src"><a title="int.MinMax.max" href="https://why3.lri.fr/stdlib/int.html#max_148">int.MinMax.max</a></code></p>
  <p>Local references <code class="src"><a title="a.A.t" href="a.A.html#t">A.t</a></code>, <code class="src"><a title="a.B.binop" href="a.B.html#binop">binop</a></code></p>
  </div>
  <pre class="src"><span class="keyword">theory</span> <a title="a.A" href="a.A.html">A</a><span title="2 parameters" class="icon remark icofont-question-circle"></span><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="a.B" href="a.B.html">B</a><span title="2 parameters" class="icon remark icofont-question-circle"></span><a href="a.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
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
  <header>Theory <code class="src"><a href="a.index.html">a</a>.A</code></header>
  <pre class="src">
  <span class="keyword">theory</span> A<span title="2 parameters" class="icon remark icofont-question-circle"></span><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
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
  <header>Module <code class="src"><a href="a.index.html">a</a>.B</code></header>
  <pre class="src">
  <span class="keyword">module</span> B<span title="2 parameters" class="icon remark icofont-question-circle"></span><a href="a.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
    <span class="keyword">val</span> <a id="binop">binop</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="a.B.t" href="#t">t</a> <a title="a.B.t" href="#t">t</a> : <a title="a.B.t" href="#t">t</a>
  </pre>
  <div class="doc">
  <p>B reference <code class="src"><a title="a.B.t" href="a.B.html#t">t</a></code></p>
  <p>A reference <code class="src"><a title="a.A.op" href="a.A.html#op">A.op</a></code></p>
  </div>
  <pre class="src">
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
