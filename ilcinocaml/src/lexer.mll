{
    open Parser

    exception SyntaxError of string
    
    (* Support functions *)

    let stringBuffer = ref (Bytes.create 2048)
    let stringEnd = ref 0

    let resetStr () = stringEnd := 0
    let addStr ch =
        let x = !stringEnd in
        let buffer = !stringBuffer
    in
        if x = String.length buffer then
            begin
                let newBuffer = Bytes.create (x*2) in
                Bytes.blit_string buffer 0 newBuffer 0 x;
                Bytes.set newBuffer x ch;
                stringBuffer := newBuffer;
                stringEnd := x+1
            end
        else
            begin
                Bytes.set buffer x ch;
                stringEnd := x+1
            end

    let getStr () = String.sub (!stringBuffer) 0 (!stringEnd)
}

rule token = parse
    (* Whitespace *)
    | [' ' '\t' '\r' '\n']      { token lexbuf }

    (* Comments *)
    | "(*"                      { comment 1 lexbuf }

    (* Reserved words *)
    | "let"                     { LET }
    | "in"                      { IN }
    | "letrec"                  { LETREC }
    | "lam"                     { LAM }
    | "nu"                      { NU }
    | "wr"                      { WR }
    | "rd"                      { RD }
    | "if"                      { IF }
    | "then"                    { THEN }
    | "else"                    { ELSE }
    | "true"                    { TRUE }
    | "false"                   { FALSE }
    | "thunk"                   { THUNK }
    | "force"                   { FORCE }
    (*| "match"                   { MATCH }
    | "with"                    { WITH }*)
    | "req"                     { REQ }

    (* Operators *)
    | "="                       { EQUAL }
    | "->"                      { RARROW }
    | "<-"                      { LARROW }
    | "!"                       { REPL }
    | "|"                       { PAR }
    | ".|"                      { PARL }
    | "&"                       { CHOICE }
    (*| "=>"                      { RRARROW }*)

    (* Arithmetic operators *)
    | "+"                       { PLUS }
    | "-"                       { MINUS }
    | "*"                       { TIMES }
    | "/"                       { DIVIDE }
    | "%"                       { MOD }
    
    (* Logical operators *)
    | "||"                      { OR }
    | "&&"                      { AND }
    | "not"                     { NOT }

    (* Relations *)
    | "<"                       { LT }
    | ">"                       { GT }
    | "<="                      { LEQ }
    | ">="                      { GEQ }
    | "=="                      { EQ }
    | "<>"                      { NEQ }
        
    (* Built-in functions *)
    | "fst"                     { FST }
    | "snd"                     { SND }
    | "rand"                    { RAND }
    | "show"                    { SHOW }
    | "::"                      { CONS }
    | "++"                      { CONCAT }
    | "lookup"                  { LOOKUP }

    (* Punctuation *)
    | "."                       { DOT }
    | "("                       { LPAREN }
    | ")"                       { RPAREN }
    | "["                       { LBRACK }
    | "]"                       { RBRACK }
    (*| "{"                       { LBRACE }
    | "}"                       { RBRACE }*)
    | ","                       { COMMA }
    | ";"                       { SEMI }
    | "_"                       { USCORE }

    (* Identifier and constants *)
    | ['a'-'z' 'A'-'Z']
      ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
                                { NAME (Lexing.lexeme lexbuf) }
    | ['?']
      ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
                                { IMPNAME (Lexing.lexeme lexbuf) }

    | ['\'']
      ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
                                { TAG (Lexing.lexeme lexbuf) }

    | ['0'-'9']+                { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "\""                      { resetStr (); string lexbuf }
    | eof                       { EOF }
and comment depth = parse
    | "(*"                      { comment (succ depth) lexbuf }
    | "*)"                      { if depth = 1
                                  then token lexbuf
                                  else comment (pred depth) lexbuf }
    | _                         { comment depth lexbuf }
and string = parse
    | '"'                       { STRING (getStr ()) }
    | '\\'                      { addStr(escaped lexbuf); string lexbuf }
    | '\n'                      { addStr '\n'; string lexbuf }
    | eof                       { raise (SyntaxError "String not terminated") }
    | _                         { addStr (Lexing.lexeme_char lexbuf 0); string lexbuf }
and escaped = parse
    | 'n'                       { '\n' }
    | 't'                       { '\t' }
    | '\\'                      { '\\' }
    | '"'                       { '\034' }
    | '\''                      { '\'' }
    | ['0'-'9'] ['0'-'9'] ['0'-'9']
      {
        let x = int_of_string (Lexing.lexeme lexbuf) in
        if x > 255 then
            raise (SyntaxError "Illegal character constant")
        else
            Char.chr x
      }
    | [^ '"' '\\' 't' 'n' '\'']
      { raise (SyntaxError "Illegal character constant") }
