require "substitution.k"

module ILC-SYNTAX
  syntax P ::= "O"
  	     | Id "[" Id "]" "." P
	     | Id "<" Id ">" "." P
	     | P "|" P
	     | "(" "new" Id ")" "(" P ")"       [binder]
	     | "!" P
	     | "(" P ")"		        [bracket]
endmodule

module ILC
  imports SUBSTITUTION
  imports ILC-SYNTAX
  syntax KVariable ::= Id
  syntax KResult ::= P
  configuration
    <T>
      <processes>
        <k multiplicity="*"> $PGM:P </k>
      </processes>
    </T>

  // Restrict inert
  rule (new X)(O) => O

  // Split parallel processes
  rule <k> P | Q => P </k> (.Bag => <k> Q </k>)

  // Send/Receive
  rule <k> C<Y>.P => P </k> 
       <k> C[X].Q => Q[Y / X] </k>

  // Restriction  
  rule <k> (new X)(P) => P </k>

  // Replication

  // Execute sends
  //rule <processes>... <k> X<Y>.P => P </k> ...</processes> <chans>... .Map => (X |-> SetItem(Y)) ...</chans>

  // Execute reads
  //rule <processes>... <k> X[Y].P => P[Z / Y] </k> ...</processes> <chans>... X |-> (Z => .) ...</chans>

endmodule
