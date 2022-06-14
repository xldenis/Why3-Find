--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc p/a.mlw
--------------------------------------------------------------------------
--- Output Directory
--------------------------------------------------------------------------
  $ find html
  html
  html/style.css
  html/p.a.html
--------------------------------------------------------------------------
--- Generated File p.a.html
--------------------------------------------------------------------------
  $ cat html/p.a.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Library <tt>p.a</tt></title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Library <tt>p.a</tt></header>
  
  <div class="doc">This is normal text.</div>
  <div class="doc">Introducing A.</div>
  <pre class="src">
  <span class="keyword">module</span> A
    <span class="keyword">use</span> int.<a href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">use</span> list.<a href="https://why3.lri.fr/stdlib/list.html#List_">List</a>
  
    <span class="keyword">type</span> <a name="t_8">t</a>
    <span class="keyword">val</span> <span class="keyword">constant</span> <a name="neutral_9">neutral</a> : <a href="#t_8">t</a> <span class="comment">(* neutral *)</span>
    <span class="keyword">val</span> <span class="keyword">function</span> <a name="op_10">op</a> <a href="#t_8">t</a> <a href="#t_8">t</a> : <a href="#t_8">t</a> <span class="comment">(* associative *)</span>
  
    <span class="keyword">let</span> <span class="keyword">rec</span> <a name="assoc_12">assoc</a> (xs : <a href="https://why3.lri.fr/stdlib/list.html#list_8">list</a> <a href="#t_8">t</a>) : <a href="#t_8">t</a> =
      <span class="keyword">variant</span> { xs }
      <span class="keyword">match</span> xs <span class="keyword">with</span>
      | <a href="https://why3.lri.fr/stdlib/list.html#Nil_8">Nil</a> -&gt; <a href="#neutral_9">neutral</a>
      | <a href="https://why3.lri.fr/stdlib/list.html#Cons_8">Cons</a> a ys -&gt; <a href="#op_10">op</a> a (assoc ys)
      <span class="keyword">end</span>
  
  <span class="keyword">end</span>
  </pre>
  <pre class="src">
  <span class="keyword">module</span> Assoc
    <span class="keyword">use</span> int.<a href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
    <span class="keyword">let</span> <span class="keyword">constant</span> <a name="one_23">one</a> = 1
    <span class="keyword">let</span> <span class="keyword">function</span> <a name="mul_24">mul</a> (a b : int) : int = a <a href="https://why3.lri.fr/stdlib/int.html#infix *_20">*</a> b
    <span class="keyword">clone</span> <a href="#A_">A</a> <span class="keyword">with</span> <span class="keyword">type</span> <a href="#t_8">t</a> = int, <span class="keyword">val</span> <a href="#neutral_9">neutral</a> = <a href="#one_23">one</a>, <span class="keyword">val</span> <a href="#op_10">op</a> = <a href="#mul_24">mul</a>
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- End of Test
--------------------------------------------------------------------------
