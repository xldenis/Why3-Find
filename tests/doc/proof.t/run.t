  $ why3find doc sequence.mlw
  $ cat html/sequence.Seq.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module sequence.Seq</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <code class="src"><a href="sequence.html">sequence</a>.Seq</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="Seq_">Seq</a>
  
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">use</span> int.<a title="int.MinMax" href="https://why3.lri.fr/stdlib/int.html#MinMax_">MinMax</a>
    <span class="keyword">use</span> option.<a title="option.Option" href="https://why3.lri.fr/stdlib/option.html#Option_">Option</a>
    <span class="keyword">use</span> list.<a title="list.ListRich" href="https://why3.lri.fr/stdlib/list.html#ListRich_">ListRich</a> <span class="keyword">as</span> L
  
    <span class="keyword">type</span> <a name="seq_8">seq</a> &#39;a = L.<a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> &#39;a
  
    <span class="keyword">let</span> <span class="keyword">rec</span> <span class="keyword">function</span> (<a name="mixfix []_10">[]</a>) (u : <a title="Seq.seq" href="#seq_8">seq</a> &#39;a) (k : int) : &#39;a
      <span class="keyword">requires</span> { 0 <a title="int.Int.(<=)" href="https://why3.lri.fr/stdlib/int.html#infix <=_25">&lt;=</a> k <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.html#infix <_21">&lt;</a> L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> u }
      <span class="keyword">ensures</span> { L.<a title="list.Nth.nth" href="https://why3.lri.fr/stdlib/list.html#nth_104">nth</a> k u = <a title="option.Option.Some" href="https://why3.lri.fr/stdlib/option.html#Some_5">Some</a> result }
      <span class="keyword">variant</span> { u }
      = <span class="keyword">match</span> u <span class="keyword">with</span> L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x w -&gt; <span class="keyword">if</span> k <a title="int.Int.(=)" href="https://why3.lri.fr/stdlib/int.html#infix =_16">=</a> 0 <span class="keyword">then</span> x <span class="keyword">else</span> w[k<a title="int.Int.(-)" href="https://why3.lri.fr/stdlib/int.html#infix -_23">-</a>1] <span class="keyword">end</span>
  
    <span class="keyword">predicate</span> (<a name="infix ==_16">==</a>) (u v : <a title="Seq.seq" href="#seq_8">seq</a> &#39;a) =
      L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> u = L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> v /\ <span class="keyword">forall</span> k. 0 <a title="int.Int.(<=)" href="https://why3.lri.fr/stdlib/int.html#infix <=_25">&lt;=</a> k <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.html#infix <_21">&lt;</a> L.<a title="list.Length.length" href="https://why3.lri.fr/stdlib/list.html#length_24">length</a> u -&gt; u[k] = v[k]
  
    <span class="keyword">lemma</span> <a name="reflexivity_19">reflexivity</a> : <span class="keyword">forall</span> u : <a title="Seq.seq" href="#seq_8">seq</a> &#39;a. u <a title="Seq.(==)" href="#infix ==_16">==</a> u
  
    <span class="keyword">let</span> <span class="keyword">rec</span> <span class="keyword">lemma</span> <a name="extensivity_21">extensivity</a> (a b : <a title="Seq.seq" href="#seq_8">seq</a> &#39;a)
      <span class="keyword">requires</span> { a <a title="Seq.(==)" href="#infix ==_16">==</a> b }
      <span class="keyword">ensures</span> { a = b }
      <span class="section level1"><span class="comment">{</span><span class="attribute section-toggle">proof</span><span class="comment section-text active">…</span><span class="comment">}</span><span class="section-text">
      <span class="keyword">variant</span> { a, b }
      = <span class="keyword">match</span> a, b <span class="keyword">with</span>
        | L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> _ a&#39; , L.<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> _ b&#39; -&gt; extensivity a&#39; b&#39;
        | _ -&gt; ()
        <span class="keyword">end</span>
      <span class="comment">{</span><span class="attribute section-toggle">qed</span><span class="comment">}</span></span></span>
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
