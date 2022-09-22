--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc p/a.mlw
--------------------------------------------------------------------------
--- Output Directory
--------------------------------------------------------------------------
  $ find html | sort
  html
  html/_p.a.html
  html/fonts
  html/fonts/icofont.woff
  html/fonts/icofont.woff2
  html/icofont.min.css
  html/p.a.A.html
  html/p.a.B.html
  html/p.a.html
  html/script.js
  html/style.css
--------------------------------------------------------------------------
--- Generated File p.a.html
--------------------------------------------------------------------------
  $ cat html/p.a.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library p.a</title>
  </head>
  <body>
  <header>Library <code>p.a</code></header>
  <div class="doc">
  <p>Before A</p>
  </div>
  <pre class="src"><span class="keyword">module</span> <a title="p.a.A" href="p.a.A.html">A</a><a href="_p.a.html#p.a" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <div class="doc">
  <p>Before B</p>
  </div>
  <pre class="src"><span class="keyword">module</span> <a title="p.a.B" href="p.a.B.html">B</a><a href="_p.a.html#p.a" title="Valid (no goals)" class="icon valid icofont-check"></a></pre>
  <div class="doc">
  <p>After B</p>
  </div>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Generated File p.a.A.html
--------------------------------------------------------------------------
  $ cat html/p.a.A.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module p.a.A</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="p.a.html">p.a</a>.A</code></header>
  <div class="doc">
  <p>Introducing A</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> <a id="A_">A</a><span title="Valid (no goals)" class="icon valid icofont-check"></span>
  
    <span class="keyword">type</span> <a id="t_6">t</a> = <a id="A_6">A</a> | <a id="B_6">B</a>
  
  </pre>
  <div class="doc">
  <p>Testing match&ndash;with</p>
  </div>
  <pre class="src">
  
    <span class="keyword">let</span> <a id="swap_9">swap</a> (e : <a title="A.t" href="#t_6">t</a>) : <a title="A.t" href="#t_6">t</a> =
      <span class="keyword">match</span> e <span class="keyword">with</span>
      | <a title="A.A" href="#A_6">A</a> -&gt; <a title="A.B" href="#B_6">B</a>
      | <a title="A.B" href="#B_6">B</a> -&gt; <a title="A.A" href="#A_6">A</a>
      <span class="keyword">end</span>
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Generated File p.a.B.html
--------------------------------------------------------------------------
  $ cat html/p.a.B.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module p.a.B</title>
  </head>
  <body>
  <header>Module <code class="src"><a href="p.a.html">p.a</a>.B</code></header>
  <div class="doc">
  <p>Introducing B</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> <a id="B_">B</a><span title="Valid (no goals)" class="icon valid icofont-check"></span>
  
    <span class="keyword">exception</span> <a id="Error_22">Error</a>
  
    <span class="keyword">val</span> <a id="wrong_24">wrong</a> () : unit
      <span class="keyword">raises</span> { <a title="B.Error" href="#Error_22">Error</a> }
  
  </pre>
  <div class="doc">
  <p>Testing try&ndash;with</p>
  </div>
  <pre class="src">
  
    <span class="keyword">let</span> <a id="test_28">test</a> () : unit =
      <span class="keyword">try</span> <a title="B.wrong" href="#wrong_24">wrong</a> () <span class="keyword">with</span> <a title="B.Error" href="#Error_22">Error</a> -&gt; () <span class="keyword">end</span>
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- End of Test
--------------------------------------------------------------------------
