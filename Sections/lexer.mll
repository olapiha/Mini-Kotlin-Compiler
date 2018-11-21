(* -lexer -*)
(*lexical analyser for Mini_Kotlin*)
{
  open Lexing
  open Parser

  exception Lexing_error of string
  let kwd_tbl =
    ["class", CLASS;
     "data", DATA;
     "else", ELSE;
     "false", FALSE;
     "fun", FUN;
     "if", IF;
     "null", NULL;
     "return", RETURN;
     "this", THIS;
     "true", TRUE;
     "val", VAL;
     "var", VAR;
     "while", WHILE;
    ]
  
  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s ->
      let s = String.lowercase_ascii s in (* la casse n'est pas significative *)
      try List.assoc s kwd_tbl with _ -> IDENT s
  }
(*Definition part*)
let letter = ['a'-'z' 'A'-'Z'] | '_'
let digit = ['0'-'9']
let ident = letter (letter | digit)*

let bit = '0' | '1'
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let integer = digit | digit (digit | '_')* digit
    	      (*find out why _ is here*)
    	      | ('0x' | '0X') (hexa | (hexa | '_')* hexa)
	      | ('0b' | '0B') (bit | bit (bit | '_')* bit)

let char = [\32 - \33 \35 - \91 \93 - \126]
	   | '\n' | ' ' | '\t' | "\\" | '"'
let string = '"' char* '"'

(*Rules*)

rule token = parse
  |  [' ' '\t' '\n']+ { token lexbuf }
  | "/*" { level := 1; comment lexbuf; token lexbuf }
  | "//" [^ '\n']*   { token lexbuf } (* one line comment *)
  | ident as id { id_or_kwd id }
  | integer as s { CST (int_of_string s) }*) 
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '{'     { BEGIN }
  | '}'     { END }
  | ','     { COMMA }
  | eof     { EOF }

and comment = parse
| "*/" { decr level; if !level > 0 then comment lexbuf }
| "/*" { incr level; comment lexbuf }
| _    { comment lexbuf }
| eof  { failwith "unterminated comment" }

