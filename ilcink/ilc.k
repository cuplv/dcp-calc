require "modules/substitution.k"

module ILC-SYNTAX
  syntax Var ::= Id
  syntax P ::= "O"
  	     | Var "(" Var ")" "." P
	     | Var "<" Var ">" "." P
	     | P "|" P
	     | "(" "new" Var ")" P
	     | "!" P
	     | "(" P ")"		[bracket]
endmodule

module ILC
  imports SUBSTITUTION
  imports ILC-SYNTAX
  syntax KResult ::= P
  configuration
    <T>
      <k> $PGM:P </k>
      <context> .List </context>
      <env> .Map </env>
    </T>


  // Split parallel processes
  rule <k> A:P => . </k> <context> . => ListItem(A) </context>
  rule <context>... ListItem(A:P | B:P) => ListItem(A) ListItem(B) ...</context>

  // Execute sends
  rule <context>... ListItem(X<Y>.P) => ListItem(P) ...</context> <env> Rho:Map (. => X |-> Y) </env>
    when notBool (X in keys(Rho))

  rule <context>... ListItem(X<Y>.P) => ListItem(P) ...</context> <env>... X |-> (_ => Y) ...</env>

  // Execute reads
  rule <context>... ListItem(X(Y).P) => ListItem(P[Z / Y]) ...</context> <env>... X |-> (Z => .) ... </env>

endmodule
