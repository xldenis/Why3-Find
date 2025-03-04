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
  <header><a href="index.html">index</a> — <code>library a</code></header>
  <div class="doc">
  <p>Testing Empty Lines</p>
  </div>
  <pre class="src"><span class="keyword">module</span> <a title="a.A" href="a.A.html">A</a><a href="a.proof.html#A" title="6 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
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
  <header><a href="index.html">index</a> — <code>library <a href="a.index.html">a</a></code> — <code>module A</code></header>
  <div class="doc">
  <p>Module A</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> A<a href="a.proof.html#A" title="6 logic parameters" class="icon small remark icofont-question-circle"></a><a href="a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
  </pre>
  <div class="doc">
  <p>Some doc</p>
  <p>More doc</p>
  </div>
  <pre class="src">
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
  </pre>
  <div class="doc">
  <p>Some doc</p>
  <p>More doc</p>
  </div>
  <pre class="src">
    <span class="keyword">type</span> <a id="u">u</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
  
    <span class="keyword">type</span> <a id="w">w</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
    <span class="keyword">type</span> <a id="k">k</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
  
    <span class="keyword">type</span> <a id="r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
  </pre>
  <div class="doc">
  <p>Doc between</p>
  </div>
  <pre class="src">
    <span class="keyword">type</span> <a id="s">s</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
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
