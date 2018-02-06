{
    open Parser

    exception SyntaxError of string

    let stringBuffer = ref (String.create 2048)
    let stringEnd = ref 0

    let resetStr () = stringEnd := 0
    let addStr ch =
        let x = !stringEnd in
        let buffer = !stringBuffer
    in
        if x = String.length buffer then
            begin
                let newBuffer = String.create (x*2) in
                String.blit buffer 0 newBuffer 0 x;
                String.set newBuffer x ch;
                stringBuffer := newBuffer;
                stringEnd := x+1
            end
        else
            begin
                String.set buffer x ch;
                stringEnd := x+1
            end

    let getStr () = String.sub (!stringBuffer) 0 (!stringEnd)
}

rule token = parse
    | "(*"                      { comment 1 lexbuf }
    (* Whitespace *)
    | [' ' '\t' '\r' '\n']      { token lexbuf }
    (* Operators *)
    | "->"                      { RARROW }
    | "<-"                      { LARROW }
    | '='                       { EQUAL }
    (*| "=>"                      { MARROW }*)
    | '!'                       { REPL }
    | "::"                      { CONS }
    | "++"                      { CONCAT }
    | '<'                       { LT }
    | '>'                       { GT }
    | "<="                      { LEQ }
    | ">="                      { GEQ }
    | "||"                      { OR }
    | "&&"                      { AND }
    | "not"                     { NOT }
    | "=="                      { EQ }
    | "<>"                      { NEQ }
    | '+'                       { PLUS }
    | '-'                       { MINUS }
    | '*'                       { TIMES }
    | '/'                       { DIVIDE }
    | '%'                       { MOD }
    | '|'                       { PAR }
    | ".|"                      { PARL }
    | '&'                       { CHOICE }
    (* Reserved words *)
    | "let"                     { LET }
    | "letrec"                  { LETREC }
    | "lam"                     { LAM }
    | "nu"                      { NU }
    | "wr"                      { WR }
    | "rd"                      { RD }
    | "in"                      { IN }
    | "if"                      { IF }
    | "then"                    { THEN }
    | "else"                    { ELSE }
    | "thunk"                   { THUNK }
    | "force"                   { FORCE }
    | "fst"                     { FST }
    | "snd"                     { SND }
    | "rand"                    { RAND }
    | "show"                    { SHOW }
    (*| "match"                   { MATCH }
    | "with"                    { WITH }*)
    (* Punctuation *)
    | '.'                       { DOT }
    | '('                       { LPAREN }
    | ')'                       { RPAREN }
    | '['                       { LBRACK }
    | ']'                       { RBRACK }
    | ','                       { COMMA }
    (* Identifiers and literals *)
    | "true"                    { TRUE }
    | "false"                   { FALSE }
    | "\""                       { resetStr (); string lexbuf }
    (*| '"'                       { read_string (Buffer.create 17) lexbuf }*)
    | ['0'-'9']+                { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | ['a'-'z' 'A'-'Z']
      ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* { NAME (Lexing.lexeme lexbuf) }
    | eof                       { EOF }
and comment depth = parse
    | "(*"                      { comment (succ depth) lexbuf }
    | "*)"                      { if depth = 1
                                  then token lexbuf
                                  else comment (pred depth) lexbuf }
    | _                         { comment depth lexbuf }
and string = parse
    | '"' { STRING (getStr ()) }
    | '\\' { addStr(escaped lexbuf); string lexbuf }
    | '\n' { addStr '\n'; string lexbuf }
    | eof { raise (SyntaxError "String not terminated") }
    | _ { addStr (Lexing.lexeme_char lexbuf 0); string lexbuf }
and escaped = parse
    | 'n' { '\n' }
    | 't' { '\t' }
    | '\\' { '\\' }
    | '"' { '\034' }
    | '\'' { '\'' }
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
(*and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }*)
