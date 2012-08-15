%{
  open Accept_types
  module Str = Re_str

  type param = Q of int | Kv of (string * pv)
  exception Invalid_input

  let get_q = function
    | (Q q)::_ -> q
    | _ -> 1000

  let get_rest = function
    | (Q _)::r | r -> List.map
        (function Kv p -> p | Q _ -> raise Invalid_input) r

  let only_q = function Q q -> q | _ -> raise Invalid_input
%}

%token STAR SLASH SEMI COMMA EQUAL EOI
%token <string> TOK QS
%token <int> INT
%token <float> FLOAT
%start media_ranges charsets encodings languages
%type <(Accept_types.q * Accept_types.media_range * Accept_types.p list) list> media_ranges
%type <(Accept_types.q * Accept_types.charset) list> charsets
%type <(Accept_types.q * Accept_types.encoding) list> encodings
%type <(Accept_types.q * Accept_types.language) list> languages
%%

param :
| SEMI TOK EQUAL QS { Kv ($2, S $4) }
| SEMI TOK EQUAL TOK { Kv ($2, T $4) }
| SEMI TOK EQUAL FLOAT {
  if $2="q" then Q (truncate (1000.*.$4)) else raise Invalid_input
}

params :
| param params { $1::$2 }
| { [] }

media_range :
| STAR SLASH STAR params { (get_q $4, AnyMedia, get_rest $4) }
| TOK SLASH STAR params { (get_q $4, AnyMediaSubtype $1, get_rest $4) }
| TOK SLASH TOK params { (get_q $4, MediaType ($1,$3), get_rest $4) }

media_ranges :
| media_range EOI { [$1] }
| media_range COMMA media_ranges { $1::$3 }
| EOI { [] }

charset :
| TOK param { (only_q $2, Charset $1) }
| STAR param { (only_q $2, AnyCharset) }

charsets :
| charset EOI { [$1] }
| charset COMMA charsets { $1::$3 }
| EOI { [] }

encoding :
| TOK param {
  (only_q $2, match $1 with
    | "gzip" -> Gzip
    | "compress" -> Compress
    | "deflate" -> Deflate
    | "identity" -> Identity
    | enc -> Encoding enc
  )
}
| STAR param { (only_q $2, AnyEncoding) }

encodings :
| encoding EOI { [$1] }
| encoding COMMA encodings { $1::$3 }
| EOI { [] }

language :
| TOK param { (only_q $2, Language (Str.split (Str.regexp "-") $1)) }
| STAR param { (only_q $2, AnyLanguage) }

languages :
| language EOI { [$1] }
| language COMMA languages { $1::$3 }
| EOI { [] }

%%
