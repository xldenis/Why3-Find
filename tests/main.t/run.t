--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc p/a.mlw
  Generated $TESTCASE_ROOT/html/index.html
--------------------------------------------------------------------------
--- Output Directory
--------------------------------------------------------------------------
  $ find html | sort
  html
  html/fonts
  html/fonts/icofont.woff
  html/fonts/icofont.woff2
  html/icofont.min.css
  html/index.html
  html/p.a.A.html
  html/p.a.B.html
  html/p.a.index.html
  html/p.a.proof.html
  html/script.js
  html/style.css
--------------------------------------------------------------------------
--- Generated index
--------------------------------------------------------------------------
  $ cat html/index.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title></title>
  </head>
  <body>
  <header>index — </header>
  <h1>Development</h1>
  <div class="doc">
  <ul>
  <li><a href="p.a.index.html">Library p.a</a><span title="1 value parameter, 0 instance found" class="icon small warning icofont-star"></span><span title="Valid (no goals)" class="icon remark icofont-check"></span></li>
  </ul>
  </div>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- Generated File p.a.html
--------------------------------------------------------------------------
  $ cat html/p.a.index.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library p.a</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library p.a</code></header>
  <div class="doc">
  <p>Before A</p>
  </div>
  <pre class="src"><span class="keyword">module</span> <a title="p.a.A" href="p.a.A.html">A</a><a href="p.a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
  <div class="doc">
  <p>Before B</p>
  </div>
  <pre class="src"><span class="keyword">module</span> <a title="p.a.B" href="p.a.B.html">B</a><a href="p.a.proof.html#B" title="1 value parameter, 0 instance found" class="icon small warning icofont-star"></a><a href="p.a.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a></pre>
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
  <header><a href="index.html">index</a> — <code>library <a href="p.a.index.html">p.a</a></code> — <code>module A</code></header>
  <div class="doc">
  <p>Introducing A</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> A<a href="p.a.proof.html#A" title="Valid (no goals)" class="icon remark icofont-check"></a>
  
    <span class="keyword">type</span> <a id="t">t</a> = <a id="A">A</a> | <a id="B">B</a>
  </pre>
  <div class="doc">
  <p>Testing match&ndash;with</p>
  </div>
  <pre class="src">
    <span class="keyword">let</span> <a id="swap">swap</a> (e : <a title="p.a.A.t" href="#t">t</a>) : <a title="p.a.A.t" href="#t">t</a> =
      <span class="keyword">match</span> e <span class="keyword">with</span>
      | <a title="p.a.A.A" href="#A">A</a> -&gt; <a title="p.a.A.B" href="#B">B</a>
      | <a title="p.a.A.B" href="#B">B</a> -&gt; <a title="p.a.A.A" href="#A">A</a>
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
  <header><a href="index.html">index</a> — <code>library <a href="p.a.index.html">p.a</a></code> — <code>module B</code></header>
  <div class="doc">
  <p>Introducing B</p>
  </div>
  <pre class="src">
  <span class="keyword">module</span> B<a href="p.a.proof.html#B" title="1 value parameter, 0 instance found" class="icon small warning icofont-star"></a><a href="p.a.proof.html#B" title="Valid (no goals)" class="icon remark icofont-check"></a>
  
    <span class="keyword">exception</span> <a id="Error">Error</a>
  
    <span class="keyword">val</span> <a id="wrong">wrong</a><span title="Value Parameter" class="icon small warning icofont-star"></span> () : unit
      <span class="keyword">raises</span> { <a title="p.a.B.Error" href="#Error">Error</a> }
  </pre>
  <div class="doc">
  <p>Testing try&ndash;with</p>
  </div>
  <pre class="src">
    <span class="keyword">let</span> <a id="test">test</a> () : unit =
      <span class="keyword">try</span> <a title="p.a.B.wrong" href="#wrong">wrong</a> () <span class="keyword">with</span> <a title="p.a.B.Error" href="#Error">Error</a> -&gt; () <span class="keyword">end</span>
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
--- End of Test
--------------------------------------------------------------------------
