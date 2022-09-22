  $ why3find doc clones.mlw certif.mlw
  $ cat html/clones.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module clones.B</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="clones.html">clones</a>.B</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a id="B_">B</a> <a href="_clones.html#" title="Valid (one goal)" class="icon valid icofont-check"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">clone</span> <a title="clones.A" href="clones.A.html#A_">A</a> <span class="section">{<span class="section-toggle">…</span><span class="section-text">
      <span class="comment section-toggle">begin</span>
        <span class="keyword">function</span> <a id="e_4">e</a> : int {<a class="attribute" title="clones.A.e" href="clones.A.html#e_4">def.</a>}
        <span class="keyword">axiom</span> <a id="neutral_6">neutral</a> {<a class="attribute" title="clones.A.neutral" href="clones.A.html#neutral_6">def.</a>}
        <span class="keyword">lemma</span> <a id="commutative_7">commutative</a><a href="_clones.html#B.A.commutative" title="Valid (one goal)" class="icon valid icofont-check"></a> {<a class="attribute" title="clones.A.commutative" href="clones.A.html#commutative_7">def.</a>}
        <span class="keyword">lemma</span> <a id="neutral_com_8">neutral_com</a> {<a class="attribute" title="clones.A.neutral_com" href="clones.A.html#neutral_com_8">def.</a>}
      <span class="comment section-toggle">end</span>
    </span>}</span><span title="Valid (one goal)" class="icon valid icofont-check"></span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="clones.A.t" href="clones.A.html#t_3">t</a> = int, <span class="keyword">function</span> <a title="clones.A.op" href="clones.A.html#op_5">op</a> = (<a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix +_19">+</a>), <span class="keyword">axiom</span> <a title="clones.A.neutral" href="clones.A.html#neutral_6">neutral</a>
    <span class="comment">(* End *)</span>
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
  <header>Module <code class="src"><a href="certif.html">certif</a>.S</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a id="S_">S</a> <a href="_certif.html#" title="Partial proof (6/10 goals)" class="icon warning icofont-exclamation-tringle"></a>
  
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">use</span> int.<a title="int.MinMax" href="https://why3.lri.fr/stdlib/int.html#MinMax_">MinMax</a>
    <span class="keyword">use</span> option.<a title="option.Option" href="https://why3.lri.fr/stdlib/option.html#Option_">Option</a>
    <span class="keyword">use</span> list.<a title="list.ListRich" href="https://why3.lri.fr/stdlib/list.html#ListRich_">ListRich</a> <span class="keyword">as</span> L
  
    <span class="keyword">type</span> <a id="seq_8">seq</a> &#39;a = L.<a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> &#39;a
  
    <span class="keyword">let</span> <span class="keyword">rec</span> <span class="keyword">function</span> (<a id="mixfix []_10">[]</a><a href="_certif.html#S.([])" title="Partial proof (3/5 goals)" class="icon warning icofont-exclamation-tringle"></a>) (u : <a title="S.seq" href="#seq_8">seq</a> &#39;a) (k : int) : &#39;a
      <span class="keyword">requires</span> { 0 <a title="int.Int.(<=)" href="https://why3.lri.fr/stdlib/int.html#infix <=_25">&lt;=</a> k <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.html#infix <_21">&lt;</a> L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> u }
      <span class="keyword">ensures</span> { L.<a title="list.Nth.nth" href="https://why3.lri.fr/stdlib/list.html#nth_104">nth</a> k u = <a title="option.Option.Some" href="https://why3.lri.fr/stdlib/option.html#Some_5">Some</a> result }
      <span class="keyword">variant</span> { u }
      = <span class="keyword">match</span> u <span class="keyword">with</span> L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x w -&gt;
          <span class="keyword">if</span> k <a title="int.Int.(=)" href="https://why3.lri.fr/stdlib/int.html#infix =_16">=</a> 1 <span class="comment">(*incorrect*)</span> <span class="keyword">then</span> x <span class="keyword">else</span> w[k<a title="int.Int.(-)" href="https://why3.lri.fr/stdlib/int.html#infix -_23">-</a>1]
        <span class="keyword">end</span>
  
    <span class="keyword">predicate</span> (<a id="infix ==_18">==</a>) (u v : <a title="S.seq" href="#seq_8">seq</a> &#39;a) =
      L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> u = L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> v /\ <span class="keyword">forall</span> k. 0 <a title="int.Int.(<=)" href="https://why3.lri.fr/stdlib/int.html#infix <=_25">&lt;=</a> k <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.html#infix <_21">&lt;</a> L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> u -&gt; u[k] = v[k]
  
    <span class="keyword">lemma</span> <a id="reflexivity_21">reflexivity</a><a href="_certif.html#S.reflexivity" title="Valid (one goal)" class="icon valid icofont-check"></a> : <span class="keyword">forall</span> u : <a title="S.seq" href="#seq_8">seq</a> &#39;a. u <a title="S.(==)" href="#infix ==_18">==</a> u
  
    <span class="keyword">let</span> <span class="keyword">rec</span> <span class="keyword">lemma</span> <a id="extensivity_23">extensivity</a><a href="_certif.html#S.extensivity" title="Partial proof (2/3 goals)" class="icon warning icofont-exclamation-tringle"></a> (a b : <a title="S.seq" href="#seq_8">seq</a> &#39;a)
      <span class="keyword">requires</span> { a <a title="S.(==)" href="#infix ==_18">==</a> b }
      <span class="keyword">ensures</span> { a = b }
      <span class="section level1"><span class="comment">{</span><span class="attribute section-toggle">proof</span><span class="comment section-text active">…</span><span class="comment">}</span><span class="section-text">
      <span class="keyword">variant</span> { a, b }
      = <span class="keyword">match</span> a, b <span class="keyword">with</span>
        | L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> _ a&#39; , L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> _ _ -&gt; extensivity a&#39; b <span class="comment">(*incorrect*)</span>
        | _ -&gt; ()
        <span class="keyword">end</span>
      <span class="comment">{</span><span class="attribute section-toggle">qed</span><span class="comment">}</span></span></span>
  
    <span class="keyword">goal</span> <a id="wrong_34">wrong</a><a href="_certif.html#S.wrong" title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></a>: 1 = 0
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
