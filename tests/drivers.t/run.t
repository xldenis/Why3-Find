  $ why3find doc -D drivers.drv drivers.mlw
  Generated $TESTCASE_ROOT/html/index.html
  $ cat html/drivers.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module drivers.A</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="drivers.index.html">drivers</a></code> — <code>module A</code></header>
  <pre class="src">
  <span class="keyword">module</span> A<a href="drivers.proof.html#A" title="3 logic parameters, 1 hypothesis, 0 instance found" class="icon small warning icofont-star"></a><a href="drivers.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-star"></span>
    <span class="keyword">val</span> <span class="keyword">function</span> <a id="add">add</a><span title="Parameter" class="icon small remark icofont-star"></span> <a title="drivers.A.t" href="#t">t</a> <a title="drivers.A.t" href="#t">t</a> : <a title="drivers.A.t" href="#t">t</a>
    <span class="keyword">predicate</span> <a id="leq">leq</a><span title="Parameter" class="icon small remark icofont-star"></span> <a title="drivers.A.t" href="#t">t</a> <a title="drivers.A.t" href="#t">t</a>
    <span class="keyword">axiom</span> <a id="increasing">increasing</a><span title="Hypothesis" class="icon small warning icofont-star"></span>: <span class="keyword">forall</span> x,y. <a title="drivers.A.leq" href="#leq">leq</a> x (<a title="drivers.A.add" href="#add">add</a> x y)
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  $ cat html/drivers.proof.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library drivers</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="drivers.index.html">drivers</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="A" href="drivers.A.html">drivers.A</a><span title="3 logic parameters, 1 hypothesis, 0 instance found" class="icon small warning icofont-star"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src">
    <span class="keyword">axiom</span> <a id="A.increasing" href="drivers.A.html#increasing">increasing</a><span title="uncloned hypothesis" class="icon small warning icofont-star"></span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
