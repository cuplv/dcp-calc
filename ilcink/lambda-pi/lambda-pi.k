require "substitution.k"

module LAMBDA-PI-SYNTAX
  syntax Val ::= Int | Id | "lam" Id "." Exp [binder]
  syntax Exp ::= Exp Exp  [strict]
         > Val
	 > Exp "+" Exp               [strict,left]
	 > Exp ".|" Exp              [left]
         > "nu" Id "." Exp [binder]
  	 | "wr" Exp "->" Exp "." Exp [strict(1,2)]
	 | "rd" Id "<-" Exp "." Exp [binder(1),strict(2)]
	 | "let" Id "=" Exp "in" Exp [binder(1),strict(2)]
	 > "(" Exp ")" [bracket]

endmodule

module LAMBDA-PI
  imports SUBSTITUTION
  imports LAMBDA-PI-SYNTAX

  syntax KVariable ::= Id
  syntax KResult ::= Val

  // Turn any parallel .| composition into separate <k> cells
  // Note that the context ~>R is kept with the left term
  rule <k>P .| Q ~> R</k> => <k>P ~> R</k> <k>Q</k> // If in a context
  rule <k>P .| Q     </k> => <k>P     </k> <k>Q</k> // If terminal

  // Exchange rule
  rule <k>wr V -> C . P => P        ...</k>
       <k>rd X <- C . Q => Q[V / X] ...</k>

  // Application
  rule (lam X . E) V => E[V/X]
  rule let X = V in E => (lam X. E) V   [structural] // sugar

  // Restriction is a no-op
  rule nu X . E => E

  // Arithmetic
  rule <k>I1 + I2 => I1 +Int I2 ...</k>

  // Clean up programs that are not the "main"
  syntax Exp ::= main(Exp) [strict] // To denote the main program
  rule <k>V:Val</k> => .Bag

  configuration <T>
     <k multiplicity="*">main($PGM:Exp):Exp</k>
   </T>
endmodule


