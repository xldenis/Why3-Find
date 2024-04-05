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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module AC</code></header>
  <pre class="src">
  <span class="keyword">module</span> AC<a href="clones.proof.html#AC" title="2 logic parameters, 2 hypotheses, 1 ground instance" class="icon small valid icofont-star"></a><a href="clones.proof.html#AC" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a id="clone-1" title="clones.Monoid" href="clones.Monoid.html#">Monoid</a><span title="2 logic parameters" class="icon small remark icofont-question-circle"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="t" title="clones.Monoid.t" href="clones.Monoid.html#t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
        <span class="keyword">function</span> <a id="op" title="clones.Monoid.op" href="clones.Monoid.html#op">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="clones.AC.t" href="clones.AC.html#t">t</a> <a title="clones.AC.t" href="clones.AC.html#t">t</a> : <a title="clones.AC.t" href="clones.AC.html#t">t</a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="comment">(* #0 *)</span>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a id="clone-3" title="clones.Commutative" href="clones.Commutative.html#">Commutative</a><span title="1 hypothesis" class="icon small warning icofont-warning-alt"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">axiom</span> <a id="commutative" title="clones.Commutative.commutative" href="clones.Commutative.html#commutative">commutative</a><span title="Hypothesis with ground instance" class="icon small valid icofont-star"></span>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">with</span> <span class="keyword">type</span> M.<a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">t</a> = <a title="clones.AC.t" href="#t">t</a>, <span class="keyword">function</span> M.<a title="clones.Commutative.M.op" href="clones.Commutative.html#M.op">op</a> = <a title="clones.AC.op" href="#op">op</a>, <span class="keyword">axiom</span> .
    <span class="keyword">clone</span> <span class="keyword">export</span> <a id="clone-5" title="clones.Associative" href="clones.Associative.html#">Associative</a><span title="1 hypothesis" class="icon small warning icofont-warning-alt"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">axiom</span> <a id="associative" title="clones.Associative.associative" href="clones.Associative.html#associative">associative</a><span title="Hypothesis with ground instance" class="icon small valid icofont-star"></span>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a><a href="#t" title="clones.AC.t" class="icon small remark icofont-question-circle"></a>, <span class="keyword">function</span> <a title="clones.Associative.op" href="clones.Associative.html#op">op</a><a href="#op" title="clones.AC.op" class="icon small remark icofont-question-circle"></a>, <span class="keyword">axiom</span> .
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module Associative</code></header>
  <pre class="src">
  <span class="keyword">module</span> Associative<a href="clones.proof.html#Associative" title="2 logic parameters, 1 hypothesis, 3 ground instances" class="icon small valid icofont-star"></a><a href="clones.proof.html#Associative" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a id="clone-1" title="clones.Monoid" href="clones.Monoid.html#">Monoid</a><span title="2 logic parameters" class="icon small remark icofont-question-circle"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="t" title="clones.Monoid.t" href="clones.Monoid.html#t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
        <span class="keyword">function</span> <a id="op" title="clones.Monoid.op" href="clones.Monoid.html#op">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a> : <a title="clones.Associative.t" href="clones.Associative.html#t">t</a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="comment">(* #0 *)</span>
    <span class="keyword">axiom</span> <a id="associative">associative</a><span title="Hypothesis with ground instance" class="icon small valid icofont-star"></span>: <span class="keyword">forall</span> x y z. <a title="clones.Associative.op" href="#op">op</a> x (<a title="clones.Associative.op" href="#op">op</a> y z) = <a title="clones.Associative.op" href="#op">op</a> (<a title="clones.Associative.op" href="#op">op</a> x y) z
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module Commutative</code></header>
  <pre class="src">
  <span class="keyword">module</span> Commutative<a href="clones.proof.html#Commutative" title="2 logic parameters, 1 hypothesis, 4 ground instances" class="icon small valid icofont-star"></a><a href="clones.proof.html#Commutative" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">clone</span> <a id="clone-1" title="clones.Monoid" href="clones.Monoid.html#">Monoid</a><span title="2 logic parameters" class="icon small remark icofont-question-circle"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="M.t" title="clones.Monoid.t" href="clones.Monoid.html#t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
        <span class="keyword">function</span> <a id="M.op" title="clones.Monoid.op" href="clones.Monoid.html#op">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">M.t</a> <a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">M.t</a> : <a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">M.t</a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> M   <span class="comment">(* #0 *)</span>
    <span class="keyword">axiom</span> <a id="commutative">commutative</a><span title="Hypothesis with ground instance" class="icon small valid icofont-star"></span>: <span class="keyword">forall</span> x y. M.<a title="clones.Commutative.M.op" href="#M.op">op</a> x y = M.<a title="clones.Commutative.M.op" href="#M.op">op</a> y x
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module IntA</code></header>
  <pre class="src">
  <span class="keyword">module</span> IntA<a href="clones.proof.html#IntA" title="Failed (no proof)" class="icon failed icofont-error"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://www.why3.org/stdlib/int.html#Int_">Int</a>
    <span class="keyword">clone</span> <a id="clone-2" title="clones.Associative" href="clones.Associative.html#">Associative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="Associative.associative" title="clones.Associative.associative" href="clones.Associative.html#associative">associative</a><a href="clones.proof.html#IntA.Associative.associative" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.Associative.t" href="clones.Associative.html#t">t</a> = int, <span class="keyword">function</span> <a title="clones.Associative.op" href="clones.Associative.html#op">op</a> = (<a title="int.Int.(+)" href="https://www.why3.org/stdlib/int.html#infix%20+_19">+</a>)
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module IntAC</code></header>
  <pre class="src">
  <span class="keyword">module</span> IntAC<a href="clones.proof.html#IntAC" title="Failed (no proof)" class="icon failed icofont-error"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://www.why3.org/stdlib/int.html#Int_">Int</a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a id="clone-6" title="clones.AC" href="clones.AC.html#">AC</a><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="commutative" title="clones.AC.commutative" href="clones.AC.html#commutative">commutative</a><a href="clones.proof.html#IntAC.commutative" title="Failed (no proof)" class="icon failed icofont-error"></a>
        <span class="keyword">lemma</span> <a id="associative" title="clones.AC.associative" href="clones.AC.html#associative">associative</a><a href="clones.proof.html#IntAC.associative" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.AC.t" href="clones.AC.html#t">t</a> = int, <span class="keyword">function</span> <a title="clones.AC.op" href="clones.AC.html#op">op</a> = (<a title="int.Int.(+)" href="https://www.why3.org/stdlib/int.html#infix%20+_19">+</a>)
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module IntC</code></header>
  <pre class="src">
  <span class="keyword">module</span> IntC<a href="clones.proof.html#IntC" title="Failed (no proof)" class="icon failed icofont-error"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://www.why3.org/stdlib/int.html#Int_">Int</a>
    <span class="keyword">clone</span> <a id="clone-2" title="clones.Commutative" href="clones.Commutative.html#">Commutative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="S.commutative" title="clones.Commutative.commutative" href="clones.Commutative.html#commutative">commutative</a><a href="clones.proof.html#IntC.S.commutative" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> S <span class="keyword">with</span> <span class="keyword">type</span> M.<a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">t</a> = int, <span class="keyword">function</span> M.<a title="clones.Commutative.M.op" href="clones.Commutative.html#M.op">op</a> = ( <a title="int.Int.(+)" href="https://www.why3.org/stdlib/int.html#infix%20+_19">+</a> )
    <span class="keyword">clone</span> <a id="clone-4" title="clones.Commutative" href="clones.Commutative.html#">Commutative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">lemma</span> <a id="P.commutative" title="clones.Commutative.commutative" href="clones.Commutative.html#commutative">commutative</a><a href="clones.proof.html#IntC.P.commutative" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> P <span class="keyword">with</span> <span class="keyword">type</span> M.<a title="clones.Commutative.M.t" href="clones.Commutative.html#M.t">t</a> = int, <span class="keyword">function</span> M.<a title="clones.Commutative.M.op" href="clones.Commutative.html#M.op">op</a> = ( <a title="int.Int.(*)" href="https://www.why3.org/stdlib/int.html#infix%20*_20">*</a> )
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module Monoid</code></header>
  <pre class="src">
  <span class="keyword">module</span> Monoid<a href="clones.proof.html#Monoid" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="clones.proof.html#Monoid" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
    <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="clones.Monoid.t" href="#t">t</a> <a title="clones.Monoid.t" href="#t">t</a> : <a title="clones.Monoid.t" href="#t">t</a>
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>module Neutral</code></header>
  <pre class="src">
  <span class="keyword">module</span> Neutral<a href="clones.proof.html#Neutral" title="3 logic parameters, 1 hypothesis, 0 instance found" class="icon small warning icofont-warning-alt"></a><a href="clones.proof.html#Neutral" title="Valid (no goals)" class="icon remark icofont-check"></a>
    <span class="keyword">clone</span> <span class="keyword">export</span> <a id="clone-1" title="clones.Monoid" href="clones.Monoid.html#">Monoid</a><span title="2 logic parameters" class="icon small remark icofont-question-circle"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">type</span> <a id="t" title="clones.Monoid.t" href="clones.Monoid.html#t">t</a><span title="Parameter" class="icon small remark icofont-question-circle"></span>
        <span class="keyword">function</span> <a id="op" title="clones.Monoid.op" href="clones.Monoid.html#op">op</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> <a title="clones.Neutral.t" href="clones.Neutral.html#t">t</a> <a title="clones.Neutral.t" href="clones.Neutral.html#t">t</a> : <a title="clones.Neutral.t" href="clones.Neutral.html#t">t</a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="comment">(* #0 *)</span>
    <span class="keyword">constant</span> <a id="e">e</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> : <a title="clones.Neutral.t" href="#t">t</a>
    <span class="keyword">axiom</span> <a id="neutral">neutral</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>: <span class="keyword">forall</span> x. <a title="clones.Neutral.op" href="#op">op</a> x <a title="clones.Neutral.e" href="#e">e</a> = <a title="clones.Neutral.op" href="#op">op</a> x <a title="clones.Neutral.e" href="#e">e</a> = x
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
  <header><a href="index.html">index</a> — <code>library clones</code></header>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Monoid" href="clones.Monoid.html">Monoid</a><a href="clones.proof.html#Monoid" title="2 logic parameters" class="icon small remark icofont-question-circle"></a><a href="clones.proof.html#Monoid" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Neutral" href="clones.Neutral.html">Neutral</a><a href="clones.proof.html#Neutral" title="3 logic parameters, 1 hypothesis, 0 instance found" class="icon small warning icofont-warning-alt"></a><a href="clones.proof.html#Neutral" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Commutative" href="clones.Commutative.html">Commutative</a><a href="clones.proof.html#Commutative" title="2 logic parameters, 1 hypothesis, 4 ground instances" class="icon small valid icofont-star"></a><a href="clones.proof.html#Commutative" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.Associative" href="clones.Associative.html">Associative</a><a href="clones.proof.html#Associative" title="2 logic parameters, 1 hypothesis, 3 ground instances" class="icon small valid icofont-star"></a><a href="clones.proof.html#Associative" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.AC" href="clones.AC.html">AC</a><a href="clones.proof.html#AC" title="2 logic parameters, 2 hypotheses, 1 ground instance" class="icon small valid icofont-star"></a><a href="clones.proof.html#AC" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.IntC" href="clones.IntC.html">IntC</a><a href="clones.proof.html#IntC" title="Failed (no proof)" class="icon failed icofont-error"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.IntA" href="clones.IntA.html">IntA</a><a href="clones.proof.html#IntA" title="Failed (no proof)" class="icon failed icofont-error"></a></pre>
  <pre class="src"><span class="keyword">module</span> <a title="clones.IntAC" href="clones.IntAC.html">IntAC</a><a href="clones.proof.html#IntAC" title="Failed (no proof)" class="icon failed icofont-error"></a></pre>
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
  <header><a href="index.html">index</a> — <code>library <a href="clones.index.html">clones</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="Monoid" href="clones.Monoid.html">clones.Monoid</a><span title="2 logic parameters" class="icon small remark icofont-question-circle"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a id="Neutral" href="clones.Neutral.html">clones.Neutral</a><span title="3 logic parameters, 1 hypothesis, 0 instance found" class="icon small warning icofont-warning-alt"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src">
    <span class="keyword">axiom</span> <a id="Neutral.neutral" href="clones.Neutral.html#neutral">neutral</a><span title="uncloned hypothesis" class="icon small warning icofont-warning-alt"></span>
  </pre>
  <pre class="src"><span class="keyword">module</span> <a id="Commutative" href="clones.Commutative.html">clones.Commutative</a><span title="2 logic parameters, 1 hypothesis, 4 ground instances" class="icon small valid icofont-star"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src">
    <span class="keyword">axiom</span> <a id="Commutative.commutative" href="clones.Commutative.html#commutative">commutative</a><span title="witnessed hypothesis" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.AC.html#clone-3">clones.AC</a><span title="sound instance" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.IntAC.html#clone-3">clones.IntAC</a><span title="sound instance" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.IntC.html#clone-2">clones.IntC</a><span title="sound instance" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.IntC.html#clone-4">clones.IntC</a><span title="sound instance" class="icon small valid icofont-star"></span>
  </pre>
  <pre class="src"><span class="keyword">module</span> <a id="Associative" href="clones.Associative.html">clones.Associative</a><span title="2 logic parameters, 1 hypothesis, 3 ground instances" class="icon small valid icofont-star"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src">
    <span class="keyword">axiom</span> <a id="Associative.associative" href="clones.Associative.html#associative">associative</a><span title="witnessed hypothesis" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.AC.html#clone-5">clones.AC</a><span title="sound instance" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.IntA.html#clone-2">clones.IntA</a><span title="sound instance" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.IntAC.html#clone-5">clones.IntAC</a><span title="sound instance" class="icon small valid icofont-star"></span>
  </pre>
  <pre class="src"><span class="keyword">module</span> <a id="AC" href="clones.AC.html">clones.AC</a><span title="2 logic parameters, 2 hypotheses, 1 ground instance" class="icon small valid icofont-star"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></pre>
  <pre class="src">
    <span class="keyword">axiom</span> <a id="AC.commutative" href="clones.AC.html#commutative">commutative</a><span title="witnessed hypothesis" class="icon small valid icofont-star"></span>
    <span class="keyword">axiom</span> <a id="AC.associative" href="clones.AC.html#associative">associative</a><span title="witnessed hypothesis" class="icon small valid icofont-star"></span>
    <span class="keyword">instance</span> <a href="clones.IntAC.html#clone-6">clones.IntAC</a><span title="sound instance" class="icon small valid icofont-star"></span>
  </pre>
  <pre class="src"><span class="keyword">module</span> <a id="IntC" href="clones.IntC.html">clones.IntC</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre>
  <pre class="src">
    <span class="keyword">goal</span> <a id="IntC.S.commutative" href="clones.IntC.html#S.commutative">S.commutative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src">
    <span class="keyword">goal</span> <a id="IntC.P.commutative" href="clones.IntC.html#P.commutative">P.commutative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src"><span class="keyword">module</span> <a id="IntA" href="clones.IntA.html">clones.IntA</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre>
  <pre class="src">
    <span class="keyword">goal</span> <a id="IntA.Associative.associative" href="clones.IntA.html#Associative.associative">Associative.associative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src"><span class="keyword">module</span> <a id="IntAC" href="clones.IntAC.html">clones.IntAC</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre>
  <pre class="src">
    <span class="keyword">goal</span> <a id="IntAC.commutative" href="clones.IntAC.html#commutative">commutative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src">
    <span class="keyword">goal</span> <a id="IntAC.associative" href="clones.IntAC.html#associative">associative</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
