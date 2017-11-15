require "substitution.k"

module ILC-SYNTAX
  syntax Var ::= Id
  syntax P ::= "O"
  	     | Var "[" Var "]" "." P
	     | Var "<" Var ">" "." P
	     | P "|" P
	     | "(" "new" Var ")" P
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
  rule <processes>... <k> X<Y>.P => P </k> ...</processes> <chans>... .Map => (X |-> Y) ...</chans>

  //@ Execute reads
  rule <processes>... <k> X[Y].P => P </k> ...</processes> <chans>... X |-> (Z => .) ...</chans>

endmodule
