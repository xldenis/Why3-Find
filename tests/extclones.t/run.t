--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc foo.mlw bar.mlw
  Generated $TESTCASE_ROOT/html/index.html
--------------------------------------------------------------------------
--- Generated Files
--------------------------------------------------------------------------
  $ cat html/foo.*.html html/bar.*.html html/*.proof.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module foo.A</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="foo.index.html">foo</a></code> — <code>module A</code></header>
  <pre class="src">
  <span class="keyword">module</span> A<a href="foo.proof.html#A" title="2 logic parameters, 1 hypothesis, 1 ground instance" class="icon small valid icofont-star"></a><a href="foo.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">type</span> <a id="a">a</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
    <span class="keyword">predicate</span> <a id="eq">eq</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> (x y: <a title="foo.A.a" href="#a">a</a>)
    <span class="keyword">axiom</span> <a id="refl">refl</a><span title="Hypothesis with ground instance" class="icon small valid icofont-star"></span>: <span class="keyword">forall</span> x y. <a title="foo.A.eq" href="#eq">eq</a> x y
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library foo</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library foo</code></header>
  <pre class="src"><span class="keyword">module</span> <a title="foo.A" href="foo.A.html">A</a><a href="foo.proof.html#A" title="2 logic parameters, 1 hypothesis, 1 ground instance" class="icon small valid icofont-star"></a><a href="foo.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library foo</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="foo.index.html">foo</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="A" href="foo.A.html">foo.A</a><span title="2 logic parameters, 1 hypothesis, 1 ground instance" class="icon small valid icofont-star"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src">
    <span class="keyword">axiom</span> <a id="A.refl" href="foo.A.html#refl">refl</a><span title="witnessed hypothesis" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="bar.B.html#clone-1">bar.B</a><span title="sound instance" class="icon small valid icofont-star"></span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module bar.B</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="bar.index.html">bar</a></code> — <code>module B</code></header>
  <pre class="src">
  <span class="keyword">module</span> B<a href="bar.proof.html#B" title="1 logic parameter" class="icon small remark icofont-question-circle"></a><a href="bar.proof.html#B" title="Failed (no proof)" class="icon failed icofont-error"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://www.why3.org/stdlib/int.html#Int_">Int</a>
  
    <span class="keyword">clone</span> foo.<a id="clone-1" title="foo.A" href="foo.A.html#">A</a><span title="1 logic parameter" class="icon small remark icofont-question-circle"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A.eq" title="foo.A.eq" href="foo.A.html#eq">eq</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int int
        <span class="keyword">lemma</span> <a id="A.refl" title="foo.A.refl" href="foo.A.html#refl">refl</a><a href="bar.proof.html#B.A.refl" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="foo.A.a" href="foo.A.html#a">a</a> = int
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library bar</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library bar</code></header>
  <pre class="src"><span class="keyword">module</span> <a title="bar.B" href="bar.B.html">B</a><a href="bar.proof.html#B" title="1 logic parameter" class="icon small remark icofont-question-circle"></a><a href="bar.proof.html#B" title="Failed (no proof)" class="icon failed icofont-error"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library bar</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="bar.index.html">bar</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="B" href="bar.B.html">bar.B</a><span title="1 logic parameter" class="icon small remark icofont-question-circle"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre>
  <pre class="src">
    <span class="keyword">goal</span> <a id="B.A.refl" href="bar.B.html#A.refl">A.refl</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library bar</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="bar.index.html">bar</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="B" href="bar.B.html">bar.B</a><span title="1 logic parameter" class="icon small remark icofont-question-circle"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre>
  <pre class="src">
    <span class="keyword">goal</span> <a id="B.A.refl" href="bar.B.html#A.refl">A.refl</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library foo</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="foo.index.html">foo</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="A" href="foo.A.html">foo.A</a><span title="2 logic parameters, 1 hypothesis, 1 ground instance" class="icon small valid icofont-star"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src">
    <span class="keyword">axiom</span> <a id="A.refl" href="foo.A.html#refl">refl</a><span title="witnessed hypothesis" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="bar.B.html#clone-1">bar.B</a><span title="sound instance" class="icon small valid icofont-star"></span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
