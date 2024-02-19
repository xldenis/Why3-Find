--------------------------------------------------------------------------
--- This is just to be sure that everything is OK with why3 cloning
--------------------------------------------------------------------------
  $ why3 prove refine.mlw
  theory Interface
    (* use why3.BuiltIn.BuiltIn *)
    
    (* use why3.Bool.Bool *)
    
    (* use why3.Unit.Unit *)
    
    (* use int.Int *)
    
    constant vA : int = 42
    
    constant cA : int
    
    axiom pA : cA > 0
    
    axiom qA : cA < 66
    
    constant cB : int
    
    constant cC : int
    
    goal cD'vc :
      (exists result:int. result > 0) /\
      (forall result:int.
        result > 0 -> (let any'result'unused = result in true))
    
    constant cD : int
    
    axiom cD'def : cD > 0
    
    goal cE'vc :
      (exists result:int. result > 0) /\
      (forall result:int.
        result > 0 -> (let any'result'unused = result in true))
    
    constant cE : int
    
    axiom cE'def : cE > 0
    
    function fY int : int
    
    axiom fY'spec : forall x:int. fY x < x
    
    goal fZ'vc :
      forall x:int.
       (exists result:int. not result = x) /\
       (forall result:int.
         not result = x -> (let any'result'unused = result in true))
    
    function fZ int : int
    
    axiom fZ'def : forall x:int. not fZ x = x
  end
  
  theory Instance
    (* use why3.BuiltIn.BuiltIn *)
    
    (* use why3.Bool.Bool *)
    
    (* use why3.Unit.Unit *)
    
    (* use int.Int *)
    
    constant cA1 : int = 42
    
    constant cB1 : int = 42
    
    constant cC1 : int = 42
    
    constant cD1 : int = 42
    
    constant cE1 : int = 42
    
    function fY1 (y:int) : int = y - 1
    
    constant vA1 : int = 42
    
    lemma pA1 : cA1 > 0
    
    axiom qA1 : cA1 < 66
    
    goal cD'refn'vc :
      (exists result:int. result > 0) -> (let result'unused = cD1 in cD1 > 0)
    
    goal cE'refn'vc :
      (exists result:int. result > 0) -> (let result'unused = cE1 in cE1 > 0)
    
    goal fX'refn'vc :
      forall x:int.
       let result = x + 1 in let fX'result'unused = result in result > x
    
    goal fY'refn'vc :
      forall x:int.
       let result = fY1 x in let fY'result'unused = result in result < x
    
    function fZ1 int : int
    
    axiom fZ'def1 : forall x:int. not fZ1 x = x
    
    (* clone Interface with function fZ = fZ1, function fY = fY1,
      constant cE = cE1, constant cD = cD1, constant cC = cC1,
      constant cB = cB1, constant cA = cA1, constant vA = vA1, prop qA = qA1,
      prop pA = pA1,  *)
  end
  
--------------------------------------------------------------------------
--- Now we run why3find and generate the doc
--------------------------------------------------------------------------
  $ why3find doc refine.mlw
  Generated $TESTCASE_ROOT/html/index.html
  $ cat html/refine.Interface.html
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <link rel="stylesheet" type="text/css" href="style.css">
  <link rel="stylesheet" type="text/css" href="icofont.min.css">
  <title>Module refine.Interface</title>
  </head>
  <body>
  <header><a href="index.html">index</a> — <code>library <a href="refine.index.html">refine</a></code> — <code>module Interface</code></header>
  <pre class="src">
  <span class="keyword">module</span> Interface<a href="refine.proof.html#Interface" title="2 value parameters, 5 logic parameters, 2 hypotheses, 1 incomplete instance" class="icon small warning icofont-warning-alt"></a><a href="refine.proof.html#Interface" title="Failed (no proof)" class="icon failed icofont-error"></a>
    <span class="keyword">use</span> int.<a title="int.Int" href="https://why3.lri.fr/stdlib/int.html#Int_">Int</a>
  </pre>
  <div class="doc">
  <p>All these are defined values, they can not be refined</p>
  </div>
  <pre class="src">
    <span class="keyword">constant</span> <a id="vA">vA</a> : int = 42
    <span class="keyword">val</span> <a id="zB">zB</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> : int
    <span class="keyword">let</span> <a id="vC">vC</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> : int = <span class="keyword">any</span> int
  </pre>
  <div class="doc">
  <p>Standard refinable constants with assumption</p>
  </div>
  <pre class="src">
    <span class="keyword">constant</span> <a id="cA">cA</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> : int
    <span class="keyword">axiom</span> <a id="pA">pA</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span> : <a title="refine.Interface.cA" href="#cA">cA</a> <a title="int.Int.(>)" href="https://why3.lri.fr/stdlib/int.html#infix%20%3E_24">&gt;</a> 0
    <span class="keyword">axiom</span> <a id="qA">qA</a><span title="Hypothesis" class="icon small warning icofont-warning-alt"></span> : <a title="refine.Interface.cA" href="#cA">cA</a> <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.html#infix%20%3C_21">&lt;</a> 66
  </pre>
  <div class="doc">
  <p>All these can be instanciated by constants or functions</p>
  </div>
  <pre class="src">
    <span class="keyword">val</span> <span class="keyword">constant</span> <a id="cB">cB</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> : int
    <span class="keyword">let</span> <span class="keyword">constant</span> <a id="cC">cC</a><span title="Parameter" class="icon small remark icofont-question-circle"></span> : int = <span class="keyword">any</span> int
    <span class="keyword">let</span> <span class="keyword">constant</span> <a id="cD">cD</a><a href="refine.proof.html#Interface.cD" title="Failed (no proof)" class="icon failed icofont-error"></a> : int = <span class="keyword">any</span> int <span class="keyword">ensures</span> { result <a title="int.Int.(>)" href="https://why3.lri.fr/stdlib/int.html#infix%20%3E_24">&gt;</a> 0 }
    <span class="keyword">let</span> <span class="keyword">function</span> <a id="cE">cE</a><a href="refine.proof.html#Interface.cE" title="Failed (no proof)" class="icon failed icofont-error"></a> : int = <span class="keyword">any</span> int <span class="keyword">ensures</span> { result <a title="int.Int.(>)" href="https://why3.lri.fr/stdlib/int.html#infix%20%3E_24">&gt;</a> 0 }
  </pre>
  <div class="doc">
  <p>All these are refinable functions</p>
  </div>
  <pre class="src">
    <span class="keyword">val</span> <a id="fX">fX</a><span title="Value Parameter" class="icon small warning icofont-warning-alt"></span> (x : int) : int <span class="keyword">ensures</span> { result <a title="int.Int.(>)" href="https://why3.lri.fr/stdlib/int.html#infix%20%3E_24">&gt;</a> x }
    <span class="keyword">val</span> <span class="keyword">function</span> <a id="fY">fY</a><span title="Value Parameter" class="icon small warning icofont-warning-alt"></span> (x : int) : int <span class="keyword">ensures</span> { result <a title="int.Int.(<)" href="https://why3.lri.fr/stdlib/int.html#infix%20%3C_21">&lt;</a> x }
  </pre>
  <div class="doc">
  <p>This function can <em>not</em> be refined</p>
  </div>
  <pre class="src">
    <span class="keyword">let</span> <span class="keyword">function</span> <a id="fZ">fZ</a><a href="refine.proof.html#Interface.fZ" title="Failed (no proof)" class="icon failed icofont-error"></a> (x : int) : int = <span class="keyword">any</span> int <span class="keyword">ensures</span> { result &lt;&gt; x }
  
  <span class="keyword">end</span>
  </pre>
  <script type="text/javascript" src="script.js"></script>
  </body>
  </html>
