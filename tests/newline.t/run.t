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
  <header>Library <a href="index.html"><code>a</code></a></header>
  <div class="doc">
  <p>Testing Empty Lines</p>
  </div>
  <pre class="src"><span class="keyword">module</span> <a title="a.A" href="a.A.html">A</a><span title="6 parameters" class="icon remark icofont-question-circle"></span><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  $ cat html/a.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module a.A</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="a.index.html">a</a>.A</code></header>
  <div class="doc">
  <p>Module A</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> A<span title="6 parameters" class="icon remark icofont-question-circle"></span><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
  </pre>
  <div class="doc">
  <p>Some doc</p>
  <p>More doc</p>
  </div>
  <pre class="src">
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
  </pre>
  <div class="doc">
  <p>Some doc</p>
  <p>More doc</p>
  </div>
  <pre class="src">
    <span class="keyword">type</span> <a id="u">u</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
  
    <span class="keyword">type</span> <a id="w">w</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
    <span class="keyword">type</span> <a id="k">k</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
  
    <span class="keyword">type</span> <a id="r">r</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
  </pre>
  <div class="doc">
  <p>Doc between</p>
  </div>
  <pre class="src">
    <span class="keyword">type</span> <a id="s">s</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
  </pre>
  <div class="doc">
  <p>End doc</p>
  </div>
  <pre class="src">
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
