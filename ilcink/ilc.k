require "substitution.k"

module ILC-SYNTAX
  syntax Var ::= Id
  syntax P ::= "O"
  	     | Var "(" Var ")" "." P
	     | Var "<" Var ">" "." P
	     | P "|" P
	     | "(" "new" Var ")" P      [binder]
	     | "!" P
	     | "(" P ")"		        [bracket]
endmodule

module ILC
  imports SUBSTITUTION
  imports ILC-SYNTAX
  syntax KResult ::= P
  configuration
    <T>
      <processes>
        <k multiplicity="*"> $PGM:P </k>
      </processes>
      <chans> .Map </chans>
    </T>

  //@ Split parallel processes
  rule <k> P | Q => P </k> (.Bag => <k> Q </k>)

  //@ Execute sends
  rule <k> X<Y>.P => P </k>

  //rule <k> A:P => . </k> <context> .Set => SetItem(A) </context>
  //rule <context>... SetItem(A | B) => SetItem(A) SetItem(B) ...</context>

/* Lists
  rule <k> A:P => . </k> <context> .List => ListItem(A) </context>
  rule <context> ListItem(A | B) => ListItem(A) ListItem(B) ...</context>
  rule <context>... ListItem(A | B) => ListItem(A) ListItem(B) </context>
*/

/*
  // Execute sends
  rule <context>... ListItem(X<Y>.P => P) ...</context> //<env> . => X |-> Y </env>

  rule <context>... ListItem(X<Y>.P) => ListItem(P) ...</context> <env>... X |-> (_ => Y) ...</env>

  // Execute reads
  rule <context>... ListItem(X(Y).P) => ListItem(P[Z / Y]) ...</context> <env>... X |-> (Z => .) ... </env>
*/
endmodule
