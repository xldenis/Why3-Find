  $ why3find doc -D drivers.drv drivers.mlw
  Generated $TESTCASE_ROOT/html
  $ cat html/drivers.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module drivers.A</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="drivers.index.html">drivers</a>.A</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a id="">A</a><span title="1 value, 2 parameters, 1 hypothesis" class="icon warning icofont-question-circle"></span><span title="Valid (no goals)" class="icon valid icofont-check"></span>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
    <span class="keyword">val</span> <span class="keyword">function</span> <a id="add">add</a><span title="Value Parameter" class="icon warning icofont-question-circle"></span> <a title="drivers.A.t" href="#t">t</a> <a title="drivers.A.t" href="#t">t</a> : <a title="drivers.A.t" href="#t">t</a>
    <span class="keyword">predicate</span> <a id="leq">leq</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="drivers.A.t" href="#t">t</a> <a title="drivers.A.t" href="#t">t</a>
    <span class="keyword">axiom</span> <a id="increasing">increasing</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span>: <span class="keyword">forall</span> x,y. <a title="drivers.A.leq" href="#leq">leq</a> x (<a title="drivers.A.add" href="#add">add</a> x y)
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
  <title>Proofs drivers</title>
  </head>
  <body>
  <header>Proofs (<code>drivers</code>)</header>
  <h1>Prover Calibration</h1>
  <pre class="src">
  </pre>
  <h1>Proof Certificates</h1>
  <pre class="src"><span class="keyword">module</span> <a href="drivers.A.html">drivers.A</a><span title="Valid (no goals)" class="icon valid icofont-check"></span></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
