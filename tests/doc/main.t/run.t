--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc p/a.mlw
--------------------------------------------------------------------------
--- Output Directory
--------------------------------------------------------------------------
  $ find html | sort
  html
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
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Library p.a</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Library <code>p.a</code></header>
  <div class="doc">
  <p>Before A</p>
  </div>
  <pre class="src module"><span class="keyword">module</span> <a title="p.a.A" href="p.a.A.html">A</a></div>
  <div class="doc">
  <p>Before B</p>
  </div>
  <pre class="src module"><span class="keyword">module</span> <a title="p.a.B" href="p.a.B.html">B</a></div>
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
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module p.a.A</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <code class="src"><a href="p.a.html">p.a</a>.A</code></header>
  <div class="doc">
  <p>Introducing A</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> <a name="A_">A</a>
  
    <span class="keyword">type</span> <a name="t_6">t</a> = <a name="A_6">A</a> | <a name="B_6">B</a>
  
  </pre>
  <div class="doc">
  <p>Testing match&ndash;with</p>
  </div>
  <pre class="src">
  
    <span class="keyword">let</span> <a name="swap_9">swap</a> (e : <a title="A.t" href="#t_6">t</a>) : <a title="A.t" href="#t_6">t</a> =
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
  <link rel="stylesheet" href="style.css" type="text/css">
  <title>Module p.a.B</title>
  </head>
  <body>
  <nav>
  </nav>
  <header>Module <code class="src"><a href="p.a.html">p.a</a>.B</code></header>
  <div class="doc">
  <p>Introducing B</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> <a name="B_">B</a>
  
    <span class="keyword">exception</span> <a name="Error_22">Error</a>
  
    <span class="keyword">val</span> <a name="wrong_24">wrong</a> () : unit
      <span class="keyword">raises</span> { <a title="B.Error" href="#Error_22">Error</a> }
  
  </pre>
  <div class="doc">
  <p>Testing try&ndash;with</p>
  </div>
  <pre class="src">
  
    <span class="keyword">let</span> <a name="test_28">test</a> () : unit =
      <span class="keyword">try</span> <a title="B.wrong" href="#wrong_24">wrong</a> () <span class="keyword">with</span> <a title="B.Error" href="#Error_22">Error</a> -&gt; () <span class="keyword">end</span>
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- End of Test
--------------------------------------------------------------------------
