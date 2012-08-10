{
type lext =
  | STAR | SLASH | SEMI | COMMA | EQUAL
  | TOK of string | QS of string | INT of int | FLOAT of float
}

(* <http://tools.ietf.org/html/rfc2616#section-2.2> *)
let token = [^'('')''<''>''@'','';'':''\\''"''/''['']''?''=''{''}'' ''\t']
  
rule media_ranges = parse
  | '*' { STAR }
  | '/' { SLASH }
  | ';' { SEMI }
  | ',' { COMMA }
  | '=' { EQUAL }
  | '\"' { List.fold_right (^) (qs [] lexbuf) "" }
  | token as tok { TOK tok }
and qs sl = parse
  | "\\\"" { qs ("\""::sl) lexbuf }
  | "\"" { sl }
  | [^'"'] as s { qs (s::sl) lexbuf }
