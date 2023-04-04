  $ why3find doc clones.mlw certif.mlw
  Generated $TESTCASE_ROOT/html
  $ cat html/clones.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module clones.A</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.A</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a id="">A</a><span title="3 parameters, 2 hypotheses" class="icon warning icofont-question-circle"></span><span title="Valid (one goal)" class="icon valid icofont-check"></span>
  
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon remark icofont-question-circle"></span>
    <span class="keyword">constant</span> <a id="e">e</a><span title="Parameter" class="icon remark icofont-question-circle"></span> : <a title="clones.A.t" href="#t">t</a>
    <span class="keyword">function</span> <a id="op">op</a><span title="Parameter" class="icon remark icofont-question-circle"></span> <a title="clones.A.t" href="#t">t</a> <a title="clones.A.t" href="#t">t</a> : <a title="clones.A.t" href="#t">t</a>
    <span class="keyword">axiom</span> <a id="neutral">neutral</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span>: <span class="keyword">forall</span> x. <a title="clones.A.op" href="#op">op</a> x <a title="clones.A.e" href="#e">e</a> = x
    <span class="keyword">axiom</span> <a id="commutative">commutative</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span>: <span class="keyword">forall</span> x y. <a title="clones.A.op" href="#op">op</a> x y = <a title="clones.A.op" href="#op">op</a> y x
    <span class="keyword">lemma</span> <a id="neutral_com%27lemma">neutral_com</a>: <span class="keyword">forall</span> x. <a title="clones.A.op" href="#op">op</a> <a title="clones.A.e" href="#e">e</a> x = x
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  $ cat html/clones.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module clones.B</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.index.html">clones</a>.B</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a id="">B</a><span title="1 parameter, 1 hypothesis" class="icon warning icofont-question-circle"></span><span title="Valid (one goal)" class="icon valid icofont-check"></span>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.Int.html#Int_">Int</a>
    <span class="keyword">clone</span> <a title="clones.A" href="clones.A.html#">A</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">function</span> <a id="A.e">A.e</a><span title="Parameter" class="icon remark icofont-question-circle"></span> : int {<a class="attribute" title="clones.A.e" href="clones.A.html#e">def.</a>}
        <span class="keyword">val</span> <span class="attribute">ghost</span> <a id="A.neutral%27lemma">A.neutral'lemma</a> int : tuple0 {<a class="attribute" title="clones.A.neutral'lemma" href="clones.A.html#neutral%27lemma">def.</a>}
        <span class="keyword">axiom</span> <a id="A.neutral">A.neutral</a><span title="Hypothesis" class="icon warning icofont-question-circle"></span> {<a class="attribute" title="clones.A.neutral" href="clones.A.html#neutral">def.</a>}
        <span class="keyword">val</span> <span class="attribute">ghost</span> <a id="A.commutative%27lemma">A.commutative'lemma</a> int int : tuple0 {<a class="attribute" title="clones.A.commutative'lemma" href="clones.A.html#commutative%27lemma">def.</a>}
        <span class="keyword">lemma</span> <a id="A.commutative">A.commutative</a><a href="_clones.html#B.A.commutative" title="Valid (one goal)" class="icon valid icofont-check"></a> {<a class="attribute" title="clones.A.commutative" href="clones.A.html#commutative">def.</a>}
        <span class="keyword">val</span> <span class="attribute">ghost</span> <a id="A.neutral_com%27lemma">A.neutral_com'lemma</a> int : tuple0 {<a class="attribute" title="clones.A.neutral_com'lemma" href="clones.A.html#neutral_com%27lemma">def.</a>}
        <span class="keyword">lemma</span> <a id="A.neutral_com">A.neutral_com</a> {<a class="attribute" title="clones.A.neutral_com" href="clones.A.html#neutral_com">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="1 parameter, 1 hypothesis" class="icon warning icofont-question-circle"></span><span title="Valid (one goal)" class="icon valid icofont-check"></span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.A.t" href="clones.A.html#t">t</a> = int, <span class="keyword">function</span> <a title="clones.A.op" href="clones.A.html#op">op</a> = (<a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20+_19">+</a>), <span class="keyword">axiom</span> <a title="clones.A.neutral" href="clones.A.html#neutral">neutral</a>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  $ cat html/certif.S.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module certif.S</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="certif.index.html">certif</a>.S</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a id="">S</a><span title="Partial proof (6/10 goals)" class="icon warning icofont-exclamation-tringle"></span>
  
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.Int.html#Int_">Int</a>
    <span class="keyword">use</span> int.<a title="int.MinMax" href="https://why3.lri.fr/stdlib/int.MinMax.html#MinMax_">MinMax</a>
    <span class="keyword">use</span> option.<a title="option.Option" href="https://why3.lri.fr/stdlib/option.Option.html#Option_">Option</a>
    <span class="keyword">use</span> list.<a title="list.ListRich" href="https://why3.lri.fr/stdlib/list.ListRich.html#ListRich_">ListRich</a> <span class="keyword">as</span> L
  
    <span class="keyword">type</span> <a id="seq">seq</a> &#39;a = L.<a title="list.List.list" href="https://why3.lri.fr/stdlib/list.List.html#list_8">list</a> &#39;a
  
    <span class="keyword">let</span> <span class="keyword">rec</span> <span class="keyword">function</span> (<a id="mixfix%20%5B%5D">[]</a><a href="_certif.html#S.mixfix%20%5B%5D" title="Partial proof (3/5 goals)" class="icon warning icofont-exclamation-tringle"></a>) (u : <a title="certif.S.seq" href="#seq">seq</a> &#39;a) (k : int) : &#39;a
      <span class="keyword">requires</span> { 0 <a title="int.Int.(<=)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20%3C=_25">&lt;=</a> k <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20%3C_21">&lt;</a> L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.Length.html#length_24">length</a> u }
      <span class="keyword">ensures</span> { L.<a title="list.Nth.nth" href="https://why3.lri.fr/stdlib/list.Nth.html#nth_104">nth</a> k u = <a title="option.Option.Some" href="https://why3.lri.fr/stdlib/option.Option.html#Some_5">Some</a> result }
      <span class="keyword">variant</span> { u }
      = <span class="keyword">match</span> u <span class="keyword">with</span> L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.List.html#Cons_8">Cons</a> x w -&gt;
          <span class="keyword">if</span> k <a title="int.Int.(=)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20=_16">=</a> 1 <span class="comment">(*incorrect*)</span> <span class="keyword">then</span> x <span class="keyword">else</span> w[k<a title="int.Int.(-)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20-_23">-</a>1]
        <span class="keyword">end</span>
  
    <span class="keyword">predicate</span> (<a id="infix%20%3D%3D">==</a>) (u v : <a title="certif.S.seq" href="#seq">seq</a> &#39;a) =
      L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.Length.html#length_24">length</a> u = L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.Length.html#length_24">length</a> v /\ <span class="keyword">forall</span> k. 0 <a title="int.Int.(<=)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20%3C=_25">&lt;=</a> k <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.Int.html#infix%20%3C_21">&lt;</a> L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.Length.html#length_24">length</a> u -&gt; u[k] = v[k]
  
    <span class="keyword">lemma</span> <a id="reflexivity%27lemma">reflexivity</a> : <span class="keyword">forall</span> u : <a title="certif.S.seq" href="#seq">seq</a> &#39;a. u <a title="certif.S.(==)" href="#infix%20%3D%3D">==</a> u
  
    <span class="keyword">let</span> <span class="keyword">rec</span> <span class="keyword">lemma</span> <a id="extensivity">extensivity</a><a href="_certif.html#S.extensivity" title="Partial proof (2/3 goals)" class="icon warning icofont-exclamation-tringle"></a> (a b : <a title="certif.S.seq" href="#seq">seq</a> &#39;a)
      <span class="keyword">requires</span> { a <a title="certif.S.(==)" href="#infix%20%3D%3D">==</a> b }
      <span class="keyword">ensures</span> { a = b }
      <span class="section level1"><span class="comment">{</span><span class="attribute section-toggle">proof</span><span class="comment section-text active">…</span><span class="comment">}</span><span class="section-text">
      <span class="keyword">variant</span> { a, b }
      = <span class="keyword">match</span> a, b <span class="keyword">with</span>
        | L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.List.html#Cons_8">Cons</a> _ a&#39; , L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.List.html#Cons_8">Cons</a> _ _ -&gt; extensivity a&#39; b <span class="comment">(*incorrect*)</span>
        | _ -&gt; ()
        <span class="keyword">end</span>
      <span class="comment">{</span><span class="attribute section-toggle">qed</span><span class="comment">}</span></span></span>
  
    <span class="keyword">goal</span> <a id="wrong">wrong</a><a href="_certif.html#S.wrong" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a>: 1 = 0
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  $ cat html/clones.proof.html
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
    alt-ergo   n=16 169ms (Alt-Ergo,2.2.0)
  </pre>
  <h1>Proof Certificates</h1>
  <pre class="src"><span class="keyword">module</span> <a href="clones.A.html">clones.A</a><span title="Valid (one goal)" class="icon valid icofont-check"></span></pre>
  <pre class="src"><span class="keyword">module</span> <a href="clones.B.html">clones.B</a><span title="Valid (one goal)" class="icon valid icofont-check"></span></pre>
  <pre class="src"> <span class="keyword">goal</span> <a id="B.A.commutative" href="clones.B.html#A.commutative">A.commutative</a><span title="Valid (one goal)" class="icon valid icofont-check"></span>
      alt-ergo 400ns</pre><script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  $ cat html/certif.proof.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Proofs certif</title>
  </head>
  <body>
  <header>Proofs (<code>certif</code>)</header>
  <h1>Prover Calibration</h1>
  <pre class="src">
    alt-ergo   n=16 169ms (Alt-Ergo,2.2.0)
    cvc4       n=42 550ms (CVC4,1.8)
    z3         n=33 570ms (Z3,4.8.6)
  </pre>
  <h1>Proof Certificates</h1>
  <pre class="src"><span class="keyword">module</span> <a href="certif.S.html">certif.S</a><span title="Partial proof (6/10 goals)" class="icon warning icofont-exclamation-tringle"></span></pre>
  <pre class="src"> <span class="keyword">goal</span> <a id="S.mixfix%20%5B%5D" href="certif.S.html#mixfix%20%5B%5D">([])</a><span title="Partial proof (3/5 goals)" class="icon warning icofont-exclamation-tringle"></span>
      split_vc<span title="Partial proof (3/5 goals)" class="icon warning icofont-exclamation-tringle"></span>
        alt-ergo 4ms
        split_vc<span title="Partial proof (1/2 goals)" class="icon warning icofont-exclamation-tringle"></span>
          stuck<span class="icon failed icofont-exclamation-circle"></span>
          alt-ergo 3ms
        alt-ergo 3ms
        stuck<span class="icon failed icofont-exclamation-circle"></span></pre><pre class="src"> <span class="keyword">goal</span> <a id="S.extensivity" href="certif.S.html#extensivity">extensivity</a><span title="Partial proof (2/3 goals)" class="icon warning icofont-exclamation-tringle"></span>
      split_vc<span title="Partial proof (2/3 goals)" class="icon warning icofont-exclamation-tringle"></span>
        alt-ergo 4ms
        stuck<span class="icon failed icofont-exclamation-circle"></span>
        cvc4 70ms</pre><pre class="src"> <span class="keyword">goal</span> <a id="S.wrong" href="certif.S.html#wrong">wrong</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span></pre><script type="text/javascript" src="script.js"></script>
  </body>
  </html>
