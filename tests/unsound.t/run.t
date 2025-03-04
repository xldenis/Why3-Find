--------------------------------------------------------------------------
--- Generating Documentation
--------------------------------------------------------------------------
  $ why3find doc unsound.mlw
  Warning, file "unsound.mlw", line 11, characters 23-48: unused variable result
  Warning, file "unsound.mlw", line 14, characters 33-58: unused variable result
  Warning, file "unsound.mlw", line 14, characters 27-30: unused variable foo
  Generated $TESTCASE_ROOT/html/index.html
--------------------------------------------------------------------------
--- Generated Files
--------------------------------------------------------------------------
  $ cat html/unsound.*.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module unsound.Dummy</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="unsound.index.html">unsound</a></code> — <code>module Dummy</code></header>
  <pre class="src">
  <span class="keyword">module</span> Dummy<a href="unsound.proof.html#Dummy" title="2 unsafe definitions" class="icon small failed icofont-warning"></a><a href="unsound.proof.html#Dummy" title="Failed (no proof)" class="icon failed icofont-error"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://www.why3.org/stdlib/int.html#Int_">Int</a>
  
    <span class="keyword">let</span> <a id="admit1">admit1</a><span title="Unsound Definition" class="icon small failed icofont-warning"></span><a href="unsound.proof.html#Dummy.admit1" title="Failed (no proof)" class="icon failed icofont-error"></a> () : unit
      <span class="keyword">ensures</span> { <span class="keyword">false</span> } = <span class="admitted">assume</span> { <span class="keyword">false</span> }
  
    <span class="keyword">let</span> <a id="admit2">admit2</a><span title="Unsound Definition" class="icon small failed icofont-warning"></span><a href="unsound.proof.html#Dummy.admit2" title="Failed (no proof)" class="icon failed icofont-error"></a> () : unit
      <span class="keyword">ensures</span> { <span class="keyword">false</span> } = <span class="keyword">val</span> foo () : unit <span class="keyword">ensures</span> { <span class="keyword">false</span> } <span class="keyword">in</span> foo ()
  
    <span class="keyword">let</span> <a id="ok_any">ok_any</a><a href="unsound.proof.html#Dummy.ok_any" title="Failed (no proof)" class="icon failed icofont-error"></a> () : unit
      <span class="keyword">ensures</span> { <span class="keyword">true</span> } = <span class="keyword">any</span> unit <span class="keyword">ensures</span> { <span class="keyword">true</span> }
  
    <span class="keyword">let</span> <a id="ok_unit">ok_unit</a><a href="unsound.proof.html#Dummy.ok_unit" title="Failed (no proof)" class="icon failed icofont-error"></a> () : unit
      <span class="keyword">ensures</span> { <span class="keyword">true</span> } = <span class="keyword">let</span> foo = <span class="keyword">any</span> unit <span class="keyword">ensures</span> { <span class="keyword">true</span> } <span class="keyword">in</span> ()
  
    <span class="keyword">let</span> <a id="ok_some">ok_some</a><a href="unsound.proof.html#Dummy.ok_some" title="Failed (no proof)" class="icon failed icofont-error"></a> () : int
      <span class="keyword">ensures</span> { result <a title="int.Int.(>=)" href="https://www.why3.org/stdlib/int.html#infix%20%3E=_26">&gt;=</a> 0 } = <span class="keyword">let</span> foo = <span class="keyword">any</span> int <span class="keyword">ensures</span> { result <a title="int.Int.(>=)" href="https://www.why3.org/stdlib/int.html#infix%20%3E=_26">&gt;=</a> 0 } <span class="keyword">in</span> foo
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library unsound</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library unsound</code></header>
  <pre class="src"><span class="keyword">module</span> <a title="unsound.Dummy" href="unsound.Dummy.html">Dummy</a><a href="unsound.proof.html#Dummy" title="2 unsafe definitions" class="icon small failed icofont-warning"></a><a href="unsound.proof.html#Dummy" title="Failed (no proof)" class="icon failed icofont-error"></a></pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Library unsound</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="unsound.index.html">unsound</a></code> — <code>proofs</code></header>
  <h1>Provers</h1>
  <pre class="src">
  </pre>
  <h1>Proofs</h1>
  <pre class="src"><span class="keyword">module</span> <a id="Dummy" href="unsound.Dummy.html">unsound.Dummy</a><span title="2 unsafe definitions" class="icon small failed icofont-warning"></span><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre>
  <pre class="src">
    <span class="keyword">unsound</span> <a id="Dummy.admit1" href="unsound.Dummy.html#admit1">admit1</a><span title="unsafe definition" class="icon small failed icofont-warning"></span>
    <span class="keyword">unsound</span> <a id="Dummy.admit2" href="unsound.Dummy.html#admit2">admit2</a><span title="unsafe definition" class="icon small failed icofont-warning"></span>
  </pre>
  <pre class="src">
    <span class="keyword">goal</span> <a id="Dummy.admit1" href="unsound.Dummy.html#admit1">admit1</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src">
    <span class="keyword">goal</span> <a id="Dummy.admit2" href="unsound.Dummy.html#admit2">admit2</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src">
    <span class="keyword">goal</span> <a id="Dummy.ok_any" href="unsound.Dummy.html#ok_any">ok_any</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src">
    <span class="keyword">goal</span> <a id="Dummy.ok_unit" href="unsound.Dummy.html#ok_unit">ok_unit</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><pre class="src">
    <span class="keyword">goal</span> <a id="Dummy.ok_some" href="unsound.Dummy.html#ok_some">ok_some</a><span title="Failed (no proof)" class="icon failed icofont-error"></span></pre><script type="text/javascript" src="script.js"></script>
  </body>
  </html>
--------------------------------------------------------------------------
