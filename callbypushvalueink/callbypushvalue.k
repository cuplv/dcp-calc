require "substitution.k"

module CALLBYPUSHVALUE
  imports SUBSTITUTION

  syntax Val ::= Id | Int
                | "thunk" Comp
		| "(" Val ")" [bracket]

  syntax TermComp ::= 
                  "ret" Val               
		> "lambda" Id "." Comp    [binder]

  syntax Comp ::= Comp "+" Comp
		| "force" Val                   [strict]
		| "let" Val "be" Id "in" Comp   [strict, binder]
  		> Comp Val                      [strict, left]
		| Comp "to" Id "." Comp         [binder]
		| TermComp
                | "(" Comp ")"            [bracket]


  syntax Item ::= Val | "to" Id "." Comp [binder]

  syntax KVariable ::= Id
  syntax KResult ::= Val | TermComp

  // Following Fig. 6 CK-machine for CBPV

  // Evaluation
  rule let V be X in M => (lambda X . M) V  [macro]
    
  rule <k> (M:Comp V:Val) => M ...</k> <stack> .List => ListItem(V) ...</stack> [pushvalue]
  rule <k> (lambda (X:Id) . (M:Comp)) => M[V / X] ...</k> <stack> ListItem(V:Val) => .List ...</stack>	[popvalue]

  // Sequencing
  rule <k> M to X . N => M </k>
       <stack> .List => ListItem(to X . N) ...</stack>  [letpush]
  rule <k> ret V => N[V / X] </k>
       <stack> ListItem(to X . N) => .List ...</stack>  [letpush]

  // Force thunk
  rule force (thunk M) => M  [force]

  // Integers
  rule ret I1 + ret I2 => ret (I1 +Int I2)      [add]

  configuration <T>
    <k> $PGM:Comp </k>
    <stack> .List </stack>
   </T>

endmodule
