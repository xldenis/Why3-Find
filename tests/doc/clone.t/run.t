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
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module a.A</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <tt><a href="a.html">a</a>.A</tt></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="A_">A</a>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">type</span> <a name="t_3">t</a>
    <span class="keyword">val</span> <span class="keyword">constant</span> <a name="neutral_4">neutral</a> : <a title="A.t" href="#t_3">t</a>
    <span class="keyword">val</span> <a name="op_5">op</a> <a title="A.t" href="#t_3">t</a> <a title="A.t" href="#t_3">t</a> : <a title="A.t" href="#t_3">t</a>
    <span class="keyword">let</span> <span class="keyword">rec</span> <a name="assoc_6">assoc</a> (xs : <a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> <a title="A.t" href="#t_3">t</a>) : <a title="A.t" href="#t_3">t</a> =
      <span class="keyword">variant</span> { xs }
      <span class="keyword">match</span> xs <span class="keyword">with</span>
      | <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a> -&gt; <a title="A.neutral" href="#neutral_4">neutral</a>
      | <a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x xs -&gt; <a title="A.op" href="#op_5">op</a> x (assoc xs)
      <span class="keyword">end</span>
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module a.B
--------------------------------------------------------------------------
  $ cat html/a.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module a.B</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <tt><a href="a.html">a</a>.B</tt></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="B_">B</a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">let</span> <span class="keyword">constant</span> <a name="zero_16">zero</a> = 0
    <span class="keyword">clone</span> <span class="keyword">export</span> <a title="a.A" href="a.A.html#A_">A</a> <span class="keyword">with</span> <span class="keyword">type</span> <a title="a.A.t" href="a.A.html#t_3">t</a> = int, <span class="keyword">val</span> <a title="a.A.neutral" href="a.A.html#neutral_4">neutral</a> = <a title="B.zero" href="#zero_16">zero</a>, <span class="keyword">val</span> <a title="a.A.op" href="a.A.html#op_5">op</a> = (<a title="int.Int.(+)" href="https://why3.lri.fr/stdlib/int.html#infix +_19">+</a>)
    <div class="clone">
      <span class="keyword">let</span> <a name="assoc_17">assoc</a> (<a title="list.List.list" href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> int) : int = {<a title="a.A.assoc" href="a.A.html#assoc_6">assoc</a>}
    </div>
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module a.E
--------------------------------------------------------------------------
  $ cat html/a.E.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module a.E</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <tt><a href="a.html">a</a>.E</tt></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="E_">E</a>
    <span class="keyword">use</span> <a title="a.B" href="a.B.html#B_">B</a>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">let</span> <a name="single_23">single</a> (x : int) : int = <a title="a.B.assoc" href="a.B.html#assoc_17">assoc</a> (<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a>)
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- Module b.C
--------------------------------------------------------------------------
  $ cat html/b.C.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module b.C</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <tt><a href="b.html">b</a>.C</tt></header>
  <pre class="src">
  <span class="keyword">module</span> <a name="C_">C</a>
    <span class="keyword">use</span> a.<a title="a.B" href="a.B.html#B_">B</a>
    <span class="keyword">use</span> list.<a title="list.List" href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
    <span class="keyword">let</span> <a name="single_4">single</a> (x : int) : int = <a title="a.B.assoc" href="a.B.html#assoc_17">assoc</a> (<a title="list.List.Cons" href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> x <a title="list.List.Nil" href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a>)
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
