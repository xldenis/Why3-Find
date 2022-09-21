  $ why3find doc a.mlw
  $ cat html/a.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module a.A</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="a.html">a</a>.A</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="A_">A</a> <span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">type</span> <a name="t_3">t</a>
    <span class="keyword">val</span> <span class="keyword">constant</span> <a name="neutral_4">neutral</a> : <a title="A.t" href="#t_3">t</a>
    <span class="keyword">val</span> <a name="op_5">op</a> <a title="A.t" href="#t_3">t</a> <a title="A.t" href="#t_3">t</a> : <a title="A.t" href="#t_3">t</a>
    <span class="keyword">let</span> <span class="keyword">rec</span> <a name="assoc_6">assoc</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span> (xs : <a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> <a title="A.t" href="#t_3">t</a>) : <a title="A.t" href="#t_3">t</a> =
      <span class="section level1"><span class="comment">{</span><span class="attribute section-toggle">proof</span><span class="comment section-text active">…</span><span class="comment">}</span><span class="section-text"> <span class="keyword">variant</span> { xs } <span class="comment">{</span><span class="attribute section-toggle">qed</span><span class="comment">}</span></span></span>
      <span class="section level1"><span class="comment">{</span><span class="attribute section-toggle">Code</span><span class="comment section-text">…</span><span class="comment">}</span><span class="section-text active">
      <span class="keyword">match</span> xs <span class="keyword">with</span>
      <span class="section level2"><span class="comment">{</span><span class="attribute section-toggle">Nil case</span><span class="comment section-text active">…</span><span class="comment">}</span><span class="section-text">
      | <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a> -&gt; <a title="A.neutral" href="#neutral_4">neutral</a> <span class="comment">{</span><span class="attribute section-toggle">…</span><span class="comment">}</span></span></span>
      <span class="section level2"><span class="comment">{</span><span class="attribute section-toggle">Cons case</span><span class="comment section-text">…</span><span class="comment">}</span><span class="section-text active">
      | <a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x xs -&gt; <a title="A.op" href="#op_5">op</a> x (assoc xs) <span class="comment">{</span><span class="attribute section-toggle">…</span><span class="comment">}</span></span></span>
      <span class="keyword">end</span>
      <span class="comment">{</span><span class="attribute section-toggle">EndCode</span><span class="comment">}</span></span></span>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
