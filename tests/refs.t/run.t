--------------------------------------------------------------------------
--- Testing References
--------------------------------------------------------------------------
  $ why3find doc main.md a.mlw
  Generated $TESTCASE_ROOT/html/index.html
  $ find html | sort
  html
  html/a.A.html
  html/a.B.html
  html/a.C.html
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
  <title>main</title>
  </head>
  <body>
  <nav>
  <a class="toc1" href="#_1">Main Document</a>
  </nav>
  <header><a href="index.html">index</a> — main
         </header>
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
  <header><a href="index.html">index</a> — <code>library a</code></header>
  <div class="doc">
  <p>Infix reference: <code class="src"><a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix%20+_19">int.Int.(+)</a></code></p>
  <p>Prefix reference: <code class="src"><a title="int.Int.(-)" href="https://why3.lri.fr/stdlib/int.html#prefix%20-_18">int.Int.(-_)</a></code></p>
  <p>Value reference: <code class="src"><a title="int.MinMax.max" href="https://why3.lri.fr/stdlib/int.html#max_78">int.MinMax.max</a></code></p>
  <p>Logic reference: <code class="src"><a title="int.MinMax.max" href="https://why3.lri.fr/stdlib/int.html#max_148">int.MinMax.max</a></code></p>
  <p>Local references <code class="src"><a title="a.A.t" href="a.A.html#t">A.t</a></code>, <code class="src"><a title="a.B.binop" href="a.B.html#binop">binop</a></code></p>
  </div>
  <pre class="src"><span class="keyword">theory</span> <a title="a.A" href="a.A.html">A</a><a href="a.proof.html#A" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="a.B" href="a.B.html">B</a><a href="a.proof.html#B" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="a.C" href="a.C.html">C</a><a href="a.proof.html#C" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#C" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
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
  <header><a href="index.html">index</a> — <code>library <a href="a.index.html">a</a></code> — <code>module A</code></header>
  <pre class="src">
  <span class="keyword">theory</span> A<a href="a.proof.html#A" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
    <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="a.A.t" href="#t">t</a> <a title="a.A.t" href="#t">t</a> : <a title="a.A.t" href="#t">t</a>
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
  <header><a href="index.html">index</a> — <code>library <a href="a.index.html">a</a></code> — <code>module B</code></header>
  <pre class="src">
  <span class="keyword">module</span> B<a href="a.proof.html#B" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
    <span class="keyword">val</span> <a id="binop">binop</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="a.B.t" href="#t">t</a> <a title="a.B.t" href="#t">t</a> : <a title="a.B.t" href="#t">t</a>
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
--- Module C
--------------------------------------------------------------------------
  $ cat html/a.C.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module a.C</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="a.index.html">a</a></code> — <code>module C</code></header>
  <pre class="src">
  <span class="keyword">module</span> C<a href="a.proof.html#C" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#C" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
    <span class="keyword">function</span> <a id="value">value</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="a.C.t" href="#t">t</a> : int
    <span class="keyword">predicate</span> (<a id="infix%20%3C%3C">&lt;&lt;</a>) (x y : <a title="a.C.t" href="#t">t</a>) = <a title="a.C.value" href="#value">value</a> x <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.html#infix%20%3C_21">&lt;</a> <a title="a.C.value" href="#value">value</a> y
    <span class="keyword">predicate</span> (<a id="infix%20%21%3D">!=</a>) (x y : <a title="a.C.t" href="#t">t</a>) = x <a title="a.C.(<<)" href="#infix%20%3C%3C">&lt;&lt;</a> y \/ y <a title="a.C.(<<)" href="#infix%20%3C%3C">&lt;&lt;</a> x
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
