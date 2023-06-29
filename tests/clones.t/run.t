--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc clones.mlw
  Generated $TESTCASE_ROOT/html/index.html
--------------------------------------------------------------------------
--- Generated Files
--------------------------------------------------------------------------
  $ cat html/clones.*.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module clones.AC</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.AC</code></header>
  <pre class="src">
  <span class="keyword">module</span> AC<span title="2 parameters, 2 hypotheses" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#AC" title="Valid (no goals)" class="icon valid icofont-check"></a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="clones.Monoid" href="clones.Monoid.html#">Monoid</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span> = {<a class="attribute" title="clones.Monoid.t" href="clones.Monoid.html#t">def.</a>}
        <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="clones.AC.t" href="clones.AC.html#t">t</a> <a title="clones.AC.t" href="clones.AC.html#t">t</a> : <a title="clones.AC.t" href="clones.AC.html#t">t</a> {<a class="attribute" title="clones.Monoid.op" href="clones.Monoid.html#op">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="2 parameters" class="icon remark icofont-question-circle"></span><span title="Valid (no goals)" class="icon valid icofont-check"></span> <span class="comment">(* #0 *)</span>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="clones.Commutative" href="clones.Commutative.html#">Commutative</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">axiom</span> <a id="commutative">commutative</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span> {<a class="attribute" title="clones.Commutative.commutative" href="clones.Commutative.html#commutative">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="1 hypothesis" class="icon warning icofont-question-circle"></span><span title="Valid (no goals)" class="icon valid icofont-check"></span> <span class="keyword">with</span> <span class="keyword">type</span> M.<a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">t</a> = <a title="clones.AC.t" href="#t">t</a>, <span class="keyword">function</span> M.<a title="clones.Commutative.M.op" href="clones.Commutative.html#M.op">op</a> = <a title="clones.AC.op" href="#op">op</a>, <span class="keyword">axiom</span> .
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="clones.Associative" href="clones.Associative.html#">Associative</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">axiom</span> <a id="associative">associative</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span> {<a class="attribute" title="clones.Associative.associative" href="clones.Associative.html#associative">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="1 hypothesis" class="icon warning icofont-question-circle"></span><span title="Valid (no goals)" class="icon valid icofont-check"></span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a>, <span class="keyword">function</span> <a title="clones.Associative.op" href="clones.Associative.html#op">op</a>, <span class="keyword">axiom</span> .
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
  <title>Module clones.Associative</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.Associative</code></header>
  <pre class="src">
  <span class="keyword">module</span> Associative<span title="2 parameters, 1 hypothesis" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#Associative" title="Valid (no goals)" class="icon valid icofont-check"></a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="clones.Monoid" href="clones.Monoid.html#">Monoid</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span> = {<a class="attribute" title="clones.Monoid.t" href="clones.Monoid.html#t">def.</a>}
        <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a> : <a title="clones.Associative.t" href="clones.Associative.html#t">t</a> {<a class="attribute" title="clones.Monoid.op" href="clones.Monoid.html#op">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="2 parameters" class="icon remark icofont-question-circle"></span><span title="Valid (no goals)" class="icon valid icofont-check"></span> <span class="comment">(* #0 *)</span>
    <span class="keyword">axiom</span> <a id="associative">associative</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span>: <span class="keyword">forall</span> x y z. <a title="clones.Associative.op" href="#op">op</a> x (<a title="clones.Associative.op" href="#op">op</a> y z) = <a title="clones.Associative.op" href="#op">op</a> (<a title="clones.Associative.op" href="#op">op</a> x y) z
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
  <title>Module clones.Commutative</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.Commutative</code></header>
  <pre class="src">
  <span class="keyword">module</span> Commutative<span title="2 parameters, 1 hypothesis" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#Commutative" title="Valid (no goals)" class="icon valid icofont-check"></a>
    <span class="keyword">clone</span> <a title="clones.Monoid" href="clones.Monoid.html#">Monoid</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="M.t">M.t</a><span title="Parameter" class="icon remark icofont-question-circle"></span> = {<a class="attribute" title="clones.Monoid.t" href="clones.Monoid.html#t">def.</a>}
        <span class="keyword">function</span> <a id="M.op">M.op</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">M.t</a> <a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">M.t</a> : <a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">M.t</a> {<a class="attribute" title="clones.Monoid.op" href="clones.Monoid.html#op">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="2 parameters" class="icon remark icofont-question-circle"></span><span title="Valid (no goals)" class="icon valid icofont-check"></span> <span class="keyword">as</span> M   <span class="comment">(* #0 *)</span>
    <span class="keyword">axiom</span> <a id="commutative">commutative</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span>: <span class="keyword">forall</span> x y. M.<a title="clones.Commutative.M.op" href="#M.op">op</a> x y = M.<a title="clones.Commutative.M.op" href="#M.op">op</a> y x
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
  <title>Module clones.IntA</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.IntA</code></header>
  <pre class="src">
  <span class="keyword">module</span> IntA<a href="clones.proof.html#IntA" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">clone</span> <a title="clones.Associative" href="clones.Associative.html#">Associative</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="Associative.associative">Associative.associative</a><a href="clones.proof.html#IntA.Associative.associative" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a> {<a class="attribute" title="clones.Associative.associative" href="clones.Associative.html#associative">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a> = int, <span class="keyword">function</span> <a title="clones.Associative.op" href="clones.Associative.html#op">op</a> = (<a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix%20+_19">+</a>)
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
  <title>Module clones.IntAC</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.IntAC</code></header>
  <pre class="src">
  <span class="keyword">module</span> IntAC<a href="clones.proof.html#IntAC" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="clones.AC" href="clones.AC.html#">AC</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="commutative">commutative</a><a href="clones.proof.html#IntAC.commutative" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a> {<a class="attribute" title="clones.AC.commutative" href="clones.AC.html#commutative">def.</a>}
        <span class="keyword">lemma</span> <a id="associative">associative</a><a href="clones.proof.html#IntAC.associative" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a> {<a class="attribute" title="clones.AC.associative" href="clones.AC.html#associative">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.AC.t" href="clones.AC.html#t">t</a> = int, <span class="keyword">function</span> <a title="clones.AC.op" href="clones.AC.html#op">op</a> = (<a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix%20+_19">+</a>)
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
  <title>Module clones.IntC</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.IntC</code></header>
  <pre class="src">
  <span class="keyword">module</span> IntC<a href="clones.proof.html#IntC" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">clone</span> <a title="clones.Commutative" href="clones.Commutative.html#">Commutative</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="S.commutative">S.commutative</a><a href="clones.proof.html#IntC.S.commutative" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a> {<a class="attribute" title="clones.Commutative.commutative" href="clones.Commutative.html#commutative">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span> <span class="keyword">as</span> S <span class="keyword">with</span> <span class="keyword">type</span> M.<a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">t</a> = int, <span class="keyword">function</span> M.<a title="clones.Commutative.M.op" href="clones.Commutative.html#M.op">op</a> = ( <a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix%20+_19">+</a> )
    <span class="keyword">clone</span> <a title="clones.Commutative" href="clones.Commutative.html#">Commutative</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="P.commutative">P.commutative</a><a href="clones.proof.html#IntC.P.commutative" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a> {<a class="attribute" title="clones.Commutative.commutative" href="clones.Commutative.html#commutative">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span> <span class="keyword">as</span> P <span class="keyword">with</span> <span class="keyword">type</span> M.<a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">t</a> = int, <span class="keyword">function</span> M.<a title="clones.Commutative.M.op" href="clones.Commutative.html#M.op">op</a> = ( <a title="int.Int.(*)" href="https://why3.lri.fr/stdlib/int.html#infix%20*_20">*</a> )
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
  <title>Module clones.Monoid</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.Monoid</code></header>
  <pre class="src">
  <span class="keyword">module</span> Monoid<span title="2 parameters" class="icon remark icofont-question-circle"></span><a href="clones.proof.html#Monoid" title="Valid (no goals)" class="icon valid icofont-check"></a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
    <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="clones.Monoid.t" href="#t">t</a> <a title="clones.Monoid.t" href="#t">t</a> : <a title="clones.Monoid.t" href="#t">t</a>
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
  <title>Module clones.Neutral</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.Neutral</code></header>
  <pre class="src">
  <span class="keyword">module</span> Neutral<span title="3 parameters, 1 hypothesis" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#Neutral" title="Valid (no goals)" class="icon valid icofont-check"></a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="clones.Monoid" href="clones.Monoid.html#">Monoid</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span> = {<a class="attribute" title="clones.Monoid.t" href="clones.Monoid.html#t">def.</a>}
        <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="clones.Neutral.t" href="clones.Neutral.html#t">t</a> <a title="clones.Neutral.t" href="clones.Neutral.html#t">t</a> : <a title="clones.Neutral.t" href="clones.Neutral.html#t">t</a> {<a class="attribute" title="clones.Monoid.op" href="clones.Monoid.html#op">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="2 parameters" class="icon remark icofont-question-circle"></span><span title="Valid (no goals)" class="icon valid icofont-check"></span> <span class="comment">(* #0 *)</span>
    <span class="keyword">constant</span> <a id="e">e</a><span title="Parameter" class="icon remark icofont-question-circle"></span> : <a title="clones.Neutral.t" href="#t">t</a>
    <span class="keyword">axiom</span> <a id="neutral">neutral</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span>: <span class="keyword">forall</span> x. <a title="clones.Neutral.op" href="#op">op</a> x <a title="clones.Neutral.e" href="#e">e</a> = <a title="clones.Neutral.op" href="#op">op</a> x <a title="clones.Neutral.e" href="#e">e</a> = x
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
  <title>Library clones</title>
  </head>
  <body>
  <header>Library <a href="index.html"><code>clones</code></a></header>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Monoid" href="clones.Monoid.html">Monoid</a><span title="2 parameters" class="icon remark icofont-question-circle"></span><a href="clones.proof.html#Monoid" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Neutral" href="clones.Neutral.html">Neutral</a><span title="3 parameters, 1 hypothesis" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#Neutral" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Commutative" href="clones.Commutative.html">Commutative</a><span title="2 parameters, 1 hypothesis" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#Commutative" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Associative" href="clones.Associative.html">Associative</a><span title="2 parameters, 1 hypothesis" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#Associative" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.AC" href="clones.AC.html">AC</a><span title="2 parameters, 2 hypotheses" class="icon warning icofont-question-circle"></span><a href="clones.proof.html#AC" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.IntC" href="clones.IntC.html">IntC</a><a href="clones.proof.html#IntC" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.IntA" href="clones.IntA.html">IntA</a><a href="clones.proof.html#IntA" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.IntAC" href="clones.IntAC.html">IntAC</a><a href="clones.proof.html#IntAC" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Proofs clones</title>
  </head>
  <body>
  <header>Proofs (<code>clones</code>)</header>
  <h1>Prover Calibration</h1>
  <pre class="src">
  </pre>
  <h1>Proof Certificates</h1>
  <pre class="src"><span class="keyword">module</span> <a id="Monoid" href="clones.Monoid.html">clones.Monoid</a><span title="Valid (no goals)" class="icon valid icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a id="Neutral" href="clones.Neutral.html">clones.Neutral</a><span title="Valid (no goals)" class="icon valid icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a id="Commutative" href="clones.Commutative.html">clones.Commutative</a><span title="Valid (no goals)" class="icon valid icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a id="Associative" href="clones.Associative.html">clones.Associative</a><span title="Valid (no goals)" class="icon valid icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a id="AC" href="clones.AC.html">clones.AC</a><span title="Valid (no goals)" class="icon valid icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a id="IntC" href="clones.IntC.html">clones.IntC</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre>
  <pre class="src"> <span class="keyword">goal</span> <a id="IntC.S.commutative" href="clones.IntC.html#S.commutative">S.commutative</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre><pre class="src"> <span class="keyword">goal</span> <a id="IntC.P.commutative" href="clones.IntC.html#P.commutative">P.commutative</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre><pre class="src"><span class="keyword">module</span> <a id="IntA" href="clones.IntA.html">clones.IntA</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre>
  <pre class="src"> <span class="keyword">goal</span> <a id="IntA.Associative.associative" href="clones.IntA.html#Associative.associative">Associative.associative</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre><pre class="src"><span class="keyword">module</span> <a id="IntAC" href="clones.IntAC.html">clones.IntAC</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre>
  <pre class="src"> <span class="keyword">goal</span> <a id="IntAC.commutative" href="clones.IntAC.html#commutative">commutative</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre><pre class="src"> <span class="keyword">goal</span> <a id="IntAC.associative" href="clones.IntAC.html#associative">associative</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre><script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
