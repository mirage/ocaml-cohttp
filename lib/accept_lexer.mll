{
  open Accept_parser
}

(* <http://tools.ietf.org/html/rfc2616#section-2.2> *)
let token = [^'('')''<''>''@'','';'':''\\''"''/''['']''?''=''{''}'' ''\t']
  
rule header_value = parse
  | '*' { STAR }
  | '/' { SLASH }
  | ';' { SEMI }
  | ',' { COMMA }
  | '=' { EQUAL }
  | '\"' { QS (List.fold_right (^) (qs [] lexbuf) "") }
  | (token)+ as tok { TOK tok }
  | ' ' { header_value lexbuf }
  | eof { EOI }
and qs sl = parse
  | "\\\"" { qs ("\""::sl) lexbuf }
  | "\"" { sl }
  | [^'"']+ as s { qs (s::sl) lexbuf }
