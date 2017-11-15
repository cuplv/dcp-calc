require "substitution.k"

module ILC-SYNTAX
  syntax P ::= "O"
  	     | Id "[" Id "]" "." P              [binder]
	     | Id "<" Id ">" "." P
	     | P "|" P
	     | "(" "new" Id ")" "(" P ")"       [binder]
	     | "!" P
	     | "(" P ")"		        [bracket]
  //syntax Val ::= "closure" "(" ID "," P ")" [function]
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
      <env> .Map </env>
      <store> .Map </store>
    </T>

  // Restrict inert
  rule (new X)(O) => O

  // Split parallel processes
  rule <k> P | Q => P </k> (.Bag => <k> Q </k>)

  rule <k> C<Y>.P => P </k> 
       <k> C[X].Q => Q </k>

  // Execute sends
  //rule <processes>... <k> X<Y>.P => P </k> ...</processes> <chans>... .Map => (X |-> SetItem(Y)) ...</chans>

  // Execute reads
  //rule <processes>... <k> X[Y].P => P[Z / Y] </k> ...</processes> <chans>... X |-> (Z => .) ...</chans>

  // New stuff
  //rule <k> (new X) P => P </k>

endmodule
