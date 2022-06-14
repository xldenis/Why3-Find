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
  <header>Library <tt>p.a</tt></header>
  <div class="doc">Before A</div>
  <div class="src module">module <tt><a href="p.a.A.html">A</a></tt></div>
  <div class="doc">Before B</div>
  <div class="src module">module <tt><a href="p.a.B.html">B</a></tt></div>
  <div class="doc">After B</div>
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
  <header>Module <tt><a href="p.a.html">p.a</a>.A</tt></header>
  <div class="doc">Introducing A</div>
  <pre class="src">
  <span class="keyword">module</span> <a name="A_">A</a>
  
    <span class="keyword">type</span> <a name="t_6">t</a> = <a name="A_6">A</a> | <a name="B_6">B</a>
  
  </pre>
  <div class="doc">Testing matchwith</div>
  <pre class="src">
  
    <span class="keyword">let</span> <a name="swap_9">swap</a> (e : <a href="#t_6">t</a>) : <a href="#t_6">t</a> =
      <span class="keyword">match</span> e <span class="keyword">with</span>
      | <a href="#A_6">A</a> -&gt; <a href="#B_6">B</a>
      | <a href="#B_6">B</a> -&gt; <a href="#A_6">A</a>
      <span class="keyword">end</span>
  
  <span class="keyword">end</span>
  </pre>
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
  <header>Module <tt><a href="p.a.html">p.a</a>.B</tt></header>
  <div class="doc">Introducing B</div>
  <pre class="src">
  <span class="keyword">module</span> <a name="B_">B</a>
  
    <span class="keyword">exception</span> <a name="Error_22">Error</a>
  
    <span class="keyword">val</span> <a name="wrong_24">wrong</a> () : unit
      <span class="keyword">raises</span> { <a href="#Error_22">Error</a> }
  
  </pre>
  <div class="doc">Testing trywith</div>
  <pre class="src">
  
    <span class="keyword">let</span> <a name="test_28">test</a> () : unit =
      <span class="keyword">try</span> <a href="#wrong_24">wrong</a> () <span class="keyword">with</span> <a href="#Error_22">Error</a> -&gt; () <span class="keyword">end</span>
  
  <span class="keyword">end</span>
  </pre>
  </body>
  </html>
--------------------------------------------------------------------------
--- End of Test
--------------------------------------------------------------------------
