--------------------------------------------------------------------------
--- Clone References
--------------------------------------------------------------------------
  $ why3find doc a.mlw b.mlw
--------------------------------------------------------------------------
--- Module a.A
--------------------------------------------------------------------------
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
    <span class="keyword">let</span> <a name="square_6">square</a> (x : <a title="A.t" href="#t_3">t</a>) : <a title="A.t" href="#t_3">t</a> = <a title="A.op" href="#op_5">op</a> x x
    <span class="keyword">let</span> <span class="keyword">rec</span> <a name="assoc_7">assoc</a><span title="Failed (no proof)" class="icon failed icofont-exclamation-circle"></span> (xs : <a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> <a title="A.t" href="#t_3">t</a>) : <a title="A.t" href="#t_3">t</a> =
      <span class="keyword">variant</span> { xs }
      <span class="keyword">match</span> xs <span class="keyword">with</span>
      | <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a> -&gt; <a title="A.neutral" href="#neutral_4">neutral</a>
      | <a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x xs -&gt; <a title="A.op" href="#op_5">op</a> x (assoc xs)
      <span class="keyword">end</span>
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module a.B
--------------------------------------------------------------------------
  $ cat html/a.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module a.B</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="a.html">a</a>.B</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="B_">B</a> <span title="Valid (no goals)" class="icon valid icofont-check"></span>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">let</span> <span class="keyword">constant</span> <a name="zero_18">zero</a> = 0
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="a.A" href="a.A.html#A_">A</a> <span class="section">{<span class="attribute section-toggle">…</span><span class="section-text"><span class="clone">
      <span class="attribute section-toggle">begin</span>
        <span class="keyword">let</span> <a name="square_19">square</a> int : int = {<a  class="attribute"title="a.A.square" href="a.A.html#square_6">def.</a>}
        <span class="keyword">let</span> <a name="assoc_19">assoc</a> (<a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> int) : int = {<a  class="attribute"title="a.A.assoc" href="a.A.html#assoc_7">def.</a>}
      <span class="attribute section-toggle">end</span></span>
    </span>}</span><span title="Valid (no goals)" class="icon valid icofont-check"></span> <span class="keyword">with</span> <span class="keyword">type</span> <a title="a.A.t" href="a.A.html#t_3">t</a> = int, <span class="keyword">val</span> <a title="a.A.neutral" href="a.A.html#neutral_4">neutral</a> = <a title="B.zero" href="#zero_18">zero</a>, <span class="keyword">val</span> <a title="a.A.op" href="a.A.html#op_5">op</a> = (<a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix +_19">+</a>)
    <span class="keyword">let</span> <a name="twice_20">twice</a> (x : int) : int = <a title="B.assoc" href="#assoc_19">assoc</a> (<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x (<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a>))
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module a.E
--------------------------------------------------------------------------
  $ cat html/a.E.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module a.E</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="a.html">a</a>.E</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="E_">E</a> <span title="Valid (no goals)" class="icon valid icofont-check"></span>
    <span class="keyword">use</span> <a title="a.B" href="a.B.html#B_">B</a>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">let</span> <a name="single_26">single</a> (x : int) : int = <a title="a.B.assoc" href="a.B.html#assoc_19">assoc</a> (<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a>)
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module b.C
--------------------------------------------------------------------------
  $ cat html/b.C.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module b.C</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="b.html">b</a>.C</code></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="C_">C</a> <span title="Valid (no goals)" class="icon valid icofont-check"></span>
    <span class="keyword">use</span> a.<a title="a.B" href="a.B.html#B_">B</a>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">let</span> <a name="single_4">single</a> (x : int) : int = <a title="a.B.assoc" href="a.B.html#assoc_19">assoc</a> (<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a>)
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
