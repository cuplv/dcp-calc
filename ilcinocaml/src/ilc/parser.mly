%{
    open Syntax
%}

/* Lexemes */
/* Identifiers and literals */
%token <int> INT
%token <Syntax.name> NAME
%token TRUE FALSE
/* Operators */
%token LARROW
%token RARROW
%token EQUAL
%token LESS
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token PAR
%token PARL
/* Reserved */
%token LET
%token LAM
%token NU
%token WR
%token RD
%token IN
%token IF THEN ELSE
/* Punctuation */
%token DOT
%token LPAREN RPAREN
%token EOF

/* Precedence and assoc */
%left PAR PARL
%nonassoc ELSE
%nonassoc LESS
%left PLUS MINUS
%left TIMES DIVIDE

%start file
%type <Syntax.process list> file
%start toplevel
%type <Syntax.process> toplevel

%%

/* Grammar */

file:
    | EOF
      { [] }
    | e = expr EOF
      { [Process e] }

toplevel:
    | e = expr EOF
      { Process e }

expr:
    | x = NAME
      { Name x }
    | n = INT
      { Int n }
    | TRUE
      { Bool true }
    | FALSE
      { Bool false }
    /* Arithmetic */
    | e1 = expr PLUS e2 = expr
      { Plus (e1, e2) }
    | e1 = expr MINUS e2 = expr
      { Minus (e1, e2) }
    | e1 = expr TIMES e2 = expr
      { Times (e1, e2) }
    | e1 = expr DIVIDE e2 = expr
      { Divide (e1, e2) }
    | e1 = expr MOD e2 = expr
      { Mod (e1, e2) }
    /* Comparison */
    | e1 = expr LESS e2 = expr
      { Less (e1, e2) }
    /* Conditionals */
    | IF b = expr THEN e1 = expr ELSE e2 = expr
      { If (b, e1, e2) }
    /* Let */
    | LET x = NAME EQUAL e1 = expr IN e2 = expr
      { Let (x, e1, e2) }
    /* App */
    | e1 = expr e2 = expr
      { App (e1, e2) }
    /* Lam */
    | LAM x = NAME DOT e = expr
      { Lam (x, e) }
    /* Wr */
    | WR e = expr RARROW x = NAME
      { Wr (e, x) }
    | RD x1 = NAME LARROW x2 = NAME
      { Rd (x1, x2) }
    | NU x = NAME DOT e = expr
      { Nu (x, e) }
    | e1 = expr PAR e2 = expr
      { ParComp (e1, e2) }
    | e1 = expr PARL e2 = expr
      { ParLeft (e1, e2) }
    | LPAREN e = expr RPAREN
      { e }
    | e1 = expr DOT e2 = expr
      { Seq (e1, e2) }
