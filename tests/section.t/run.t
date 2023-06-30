  $ why3find doc a.mlw
  Generated $TESTCASE_ROOT/html/index.html
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
  <pre class="src">
  <span class="keyword">module</span> A<span title="3 parameters" class="icon small remark icofont-star"></span><a href="a.proof.html#A" title="Failed (no proof)" class="icon failed icofont-warning"></a>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">type</span> <a id="t">t</a><span title="Parameter" class="icon small remark icofont-star"></span>
    <span class="keyword">val</span> <span class="keyword">constant</span> <a id="neutral">neutral</a><span title="Parameter" class="icon small remark icofont-star"></span> : <a title="a.A.t" href="#t">t</a>
    <span class="keyword">val</span> <a id="op">op</a><span title="Parameter" class="icon small remark icofont-star"></span> <a title="a.A.t" href="#t">t</a> <a title="a.A.t" href="#t">t</a> : <a title="a.A.t" href="#t">t</a>
    <span class="keyword">let</span> <span class="keyword">rec</span> <a id="assoc">assoc</a><a href="a.proof.html#A.assoc" title="Failed (no proof)" class="icon failed icofont-warning"></a> (xs : <a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> <a title="a.A.t" href="#t">t</a>) : <a title="a.A.t" href="#t">t</a> =
      <span class="section level1"><span class="comment">{</span><span class="attribute section-toggle">proof</span><span class="comment section-text active">…</span><span class="comment">}</span><span class="section-text"> <span class="keyword">variant</span> { xs } <span class="comment">{</span><span class="attribute section-toggle">qed</span><span class="comment">}</span></span></span>
      <span class="section level1"><span class="comment">{</span><span class="attribute section-toggle">Code</span><span class="comment section-text">…</span><span class="comment">}</span><span class="section-text active">
      <span class="keyword">match</span> xs <span class="keyword">with</span>
      <span class="section level2"><span class="comment">{</span><span class="attribute section-toggle">Nil case</span><span class="comment section-text active">…</span><span class="comment">}</span><span class="section-text">
      | <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a> -&gt; <a title="a.A.neutral" href="#neutral">neutral</a> <span class="comment">{</span><span class="attribute section-toggle">…</span><span class="comment">}</span></span></span>
      <span class="section level2"><span class="comment">{</span><span class="attribute section-toggle">Cons case</span><span class="comment section-text">…</span><span class="comment">}</span><span class="section-text active">
      | <a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x xs -&gt; <a title="a.A.op" href="#op">op</a> x (assoc xs) <span class="comment">{</span><span class="attribute section-toggle">…</span><span class="comment">}</span></span></span>
      <span class="keyword">end</span>
      <span class="comment">{</span><span class="attribute section-toggle">EndCode</span><span class="comment">}</span></span></span>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
