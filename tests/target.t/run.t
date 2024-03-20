--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc target.mlw
  Generated $TESTCASE_ROOT/html/index.html
--------------------------------------------------------------------------
--- Generated Files
--------------------------------------------------------------------------
  $ cat html/target.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module target.B</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="target.index.html">target</a></code> — <code>module B</code></header>
  <pre class="src">
  <span class="keyword">module</span> B<a href="target.proof.html#B" title="7 logic parameters, 9 hypotheses, 0 instance found" class="icon small warning icofont-warning-alt"></a><a href="target.proof.html#B" title="Failed (no proof)" class="icon failed icofont-error"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://www.why3.org/stdlib/int.html#Int_">Int</a>
    <span class="keyword">use</span> <a title="target.Q" href="target.Q.html#">Q</a>
    <span class="keyword">type</span> <a id="t">t</a> = int
    <span class="keyword">predicate</span> <a id="p">p</a> (x : int) = x <a title="int.Int.(>)" href="https://www.why3.org/stdlib/int.html#infix%20%3E_24">&gt;</a> 0
    <span class="keyword">clone</span> <a id="clone-1" title="target.A" href="target.A.html#">A</a><span title="1 logic parameter, 2 hypotheses" class="icon small warning icofont-warning-alt"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A0.r" title="target.A.r" href="target.A.html#r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int
        <span class="keyword">axiom</span> <a id="A0.pq" title="target.A.pq" href="target.A.html#pq">pq</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
        <span class="keyword">axiom</span> <a id="A0.qr" title="target.A.qr" href="target.A.html#qr">qr</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> A0 <span class="keyword">with</span> <span class="keyword">type</span> <a title="target.A.a" href="target.A.html#a">a</a> = int, <span class="keyword">predicate</span> <a title="target.A.q" href="target.A.html#q">q</a> = Q.<a title="target.Q.q" href="target.Q.html#q">q</a>, <span class="keyword">predicate</span> <a title="target.A.p" href="target.A.html#p">p</a><a href="#p" title="target.B.p" class="icon small remark icofont-question-circle"></a>, <span class="keyword">axiom</span> .
    <span class="keyword">clone</span> <a id="clone-2" title="target.A" href="target.A.html#">A</a><span title="1 logic parameter, 1 hypothesis" class="icon small warning icofont-warning-alt"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A1.r" title="target.A.r" href="target.A.html#r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int
        <span class="keyword">axiom</span> <a id="A1.pq" title="target.A.pq" href="target.A.html#pq">pq</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
        <span class="keyword">lemma</span> <a id="A1.qr" title="target.A.qr" href="target.A.html#qr">qr</a><a href="target.proof.html#B.A1.qr" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> A1 <span class="keyword">with</span> <span class="keyword">type</span> <a title="target.A.a" href="target.A.html#a">a</a> = int, <span class="keyword">predicate</span> <a title="target.A.q" href="target.A.html#q">q</a> = Q.<a title="target.Q.q" href="target.Q.html#q">q</a>, <span class="keyword">predicate</span> <a title="target.A.p" href="target.A.html#p">p</a><a href="#p" title="target.B.p" class="icon small remark icofont-question-circle"></a>, <span class="keyword">axiom</span> <a title="target.A.pq" href="target.A.html#pq">pq</a>
    <span class="keyword">clone</span> <a id="clone-3" title="target.A" href="target.A.html#">A</a><span title="1 logic parameter, 1 hypothesis" class="icon small warning icofont-warning-alt"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A2.r" title="target.A.r" href="target.A.html#r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int
        <span class="keyword">axiom</span> <a id="A2.pq" title="target.A.pq" href="target.A.html#pq">pq</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
        <span class="keyword">lemma</span> <a id="A2.qr" title="target.A.qr" href="target.A.html#qr">qr</a><a href="target.proof.html#B.A2.qr" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> A2 <span class="keyword">with</span> <span class="keyword">type</span> <a title="target.A.a" href="target.A.html#a">a</a> = int, <span class="keyword">predicate</span> <a title="target.A.p" href="target.A.html#p">p</a><a href="#p" title="target.B.p" class="icon small remark icofont-question-circle"></a>, <span class="keyword">axiom</span> <a title="target.A.pq" href="target.A.html#pq">pq</a>, <span class="keyword">predicate</span> <a title="target.A.q" href="target.A.html#q">q</a> = Q.<a title="target.Q.q" href="target.Q.html#q">q</a>
    <span class="keyword">clone</span> <a id="clone-4" title="target.A" href="target.A.html#">A</a><span title="1 logic parameter, 1 hypothesis" class="icon small warning icofont-warning-alt"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A3.r" title="target.A.r" href="target.A.html#r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int
        <span class="keyword">axiom</span> <a id="A3.pq" title="target.A.pq" href="target.A.html#pq">pq</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
        <span class="keyword">lemma</span> <a id="A3.qr" title="target.A.qr" href="target.A.html#qr">qr</a><a href="target.proof.html#B.A3.qr" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> A3 <span class="keyword">with</span> <span class="keyword">type</span> <a title="target.A.a" href="target.A.html#a">a</a> = int, <span class="keyword">axiom</span> <a title="target.A.pq" href="target.A.html#pq">pq</a>, <span class="keyword">predicate</span> <a title="target.A.q" href="target.A.html#q">q</a> = Q.<a title="target.Q.q" href="target.Q.html#q">q</a>, <span class="keyword">predicate</span> <a title="target.A.p" href="target.A.html#p">p</a><a href="#p" title="target.B.p" class="icon small remark icofont-question-circle"></a>
    <span class="keyword">clone</span> <a id="clone-5" title="target.A" href="target.A.html#">A</a><span title="1 logic parameter, 1 hypothesis" class="icon small warning icofont-warning-alt"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A4.r" title="target.A.r" href="target.A.html#r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int
        <span class="keyword">axiom</span> <a id="A4.pq" title="target.A.pq" href="target.A.html#pq">pq</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
        <span class="keyword">lemma</span> <a id="A4.qr" title="target.A.qr" href="target.A.html#qr">qr</a><a href="target.proof.html#B.A4.qr" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> A4 <span class="keyword">with</span> <span class="keyword">type</span> <a title="target.A.a" href="target.A.html#a">a</a> = int, <span class="keyword">axiom</span> <a title="target.A.pq" href="target.A.html#pq">pq</a>, <span class="keyword">predicate</span> <a title="target.A.q" href="target.A.html#q">q</a> = Q.<a title="target.Q.q" href="target.Q.html#q">q</a>, <span class="keyword">predicate</span> <a title="target.A.p" href="target.A.html#p">p</a><a href="#p" title="target.B.p" class="icon small remark icofont-question-circle"></a> <span class="comment">(* spaced *)</span>
    <span class="keyword">clone</span> <a id="clone-6" title="target.A" href="target.A.html#">A</a><span title="1 logic parameter, 1 hypothesis" class="icon small warning icofont-warning-alt"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A5.r" title="target.A.r" href="target.A.html#r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int
        <span class="keyword">axiom</span> <a id="A5.pq" title="target.A.pq" href="target.A.html#pq">pq</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
        <span class="keyword">lemma</span> <a id="A5.qr" title="target.A.qr" href="target.A.html#qr">qr</a><a href="target.proof.html#B.A5.qr" title="Failed (no proof)" class="icon failed icofont-error"></a>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> A5 <span class="keyword">with</span> <span class="keyword">predicate</span> <a title="target.A.p" href="target.A.html#p">p</a><a href="#p" title="target.B.p" class="icon small remark icofont-question-circle"></a>, <span class="keyword">axiom</span> <a title="target.A.pq" href="target.A.html#pq">pq</a>, <span class="keyword">predicate</span> <a title="target.A.q" href="target.A.html#q">q</a> = Q.<a title="target.Q.q" href="target.Q.html#q">q</a>, <span class="keyword">type</span> <a title="target.A.a" href="target.A.html#a">a</a> = int
    <span class="keyword">clone</span> <a id="clone-7" title="target.A" href="target.A.html#">A</a><span title="1 logic parameter, 2 hypotheses" class="icon small warning icofont-warning-alt"></span><span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">predicate</span> <a id="A6.r" title="target.A.r" href="target.A.html#r">r</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> int
        <span class="keyword">axiom</span> <a id="A6.pq" title="target.A.pq" href="target.A.html#pq">pq</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
        <span class="keyword">axiom</span> <a id="A6.qr" title="target.A.qr" href="target.A.html#qr">qr</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span>
      <span class="comment section-toggle">end</span>
    </span>}</span> <span class="keyword">as</span> A6 <span class="keyword">with</span> <span class="keyword">predicate</span> <a title="target.A.p" href="target.A.html#p">p</a><a href="#p" title="target.B.p" class="icon small remark icofont-question-circle"></a>, <span class="keyword">axiom</span> ., <span class="keyword">predicate</span> <a title="target.A.q" href="target.A.html#q">q</a> = Q.<a title="target.Q.q" href="target.Q.html#q">q</a>, <span class="keyword">type</span> <a title="target.A.a" href="target.A.html#a">a</a> = int
    <span class="keyword">predicate</span> <a id="r">r</a> (x : int) = x <a title="int.Int.(<)" href="https://www.why3.org/stdlib/int.html#infix%20%3C_21">&lt;</a> 0
    <span class="keyword">lemma</span> <a id="pr">pr</a><a href="target.proof.html#B.pr" title="Failed (no proof)" class="icon failed icofont-error"></a>: <span class="keyword">forall</span> x. <a title="target.B.p" href="#p">p</a> x &lt;-&gt; <a title="target.B.r" href="#r">r</a> (<a title="int.Int.(-)" href="https://www.why3.org/stdlib/int.html#prefix%20-_18">-</a>x)
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
