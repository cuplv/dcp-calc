require "substitution.k"

module CALLBYPUSHVALUE
  imports SUBSTITUTION

  syntax Val ::= Id | Int
                | "thunk" Comp
		| "(" Val ")" [bracket]

  syntax TerminalComp ::= 
                  "ret" Val               
		> "lambda" Id "." Comp    [binder]

  syntax Comp ::= Comp Val    	  [left]
                > Comp "+" Comp
		| Val "+" Val
		| "force" Val                 
		| "let" Id "be" Val "in" Comp   [binder]
		| "let" Id "be" Comp "." Comp   [binder]
		| TerminalComp
                | "(" Comp ")"            [bracket]


  syntax StackItem ::= Val | "to" Id "." Comp

  syntax KVariable ::= Id
  syntax KResult ::= TerminalComp

  // For fresh variables
  syntax Id ::= "$x" | "$y"

  // Following Fig. 6 CK-machine for CBPV

  // Evaluation
  rule let X be V in M => M[V / X]
  rule <k> M:Comp V:Val => M </k>
       <stack> .List => ListItem(V) ...</stack>		[pushvalue]
  rule <k> (lambda (X:Id) . (M:Comp)) => M[V / X] ...</k>
       <stack> ListItem(V:Val) => .List ...</stack>	[popvalue]

  // Sequencing
  rule <k> let X be M . N => M </k>
       <stack> .List => ListItem(to X . N) ...</stack>  [letpush]
  rule <k> ret V => N[V / X] </k>
       <stack> ListItem(to X . N) => .List ...</stack>  [letpush]

  // Force thunk
  rule force thunk M => M  [force]

  // Integers
  rule I1 + I2 => ret (I1 +Int I2)      [add]
  // Syntax sugar for composing computations
  rule M:Comp + N:Comp => let $x be M . let $y be N . ($x + $y)
   
  configuration <T>
    <k> $PGM:Comp </k>
    <stack> .List </stack>
   </T>

endmodule
