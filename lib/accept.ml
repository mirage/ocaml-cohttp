open Genlex
open Printf
module Str = Re_str

type pv = T of string | S of string
type p = string * pv
type ctp =
  | Type of string * string * p list
  | AnySubtype of string * p list
  | Any of p list
type qtp = int * ctp

let lexer s = make_lexer ["="; ";"; "/"; ","] (Stream.of_string s)

let q_of_float q = truncate (1000.*.q)

let quote = Str.global_replace (Str.regexp "\"") "\\\""

let rec string_of_pl = function
  | [] -> ""
  | (k,T v)::r -> sprintf ";%s=%s%s" k v (string_of_pl r)
  | (k,S v)::r -> sprintf ";%s=\"%s\"%s" k (quote v) (string_of_pl r)

let string_of_qtp = function
  | (q,Type (t,st,pl)) ->
      sprintf "%s/%s;q=%.3f%s" t st ((float q)/.1000.) (string_of_pl pl)
  | (q,AnySubtype (t,pl)) ->
      sprintf "%s/*;q=%.3f%s" t ((float q)/.1000.) (string_of_pl pl)
  | (q,Any pl) -> sprintf "*/*;q=%.3f%s" ((float q)/.1000.) (string_of_pl pl)

let string_of_types =
  let rec aux s = function
    | qtp::[] -> s^(string_of_qtp qtp)
    | [] -> s
    | qtp::r -> s^(string_of_qtp qtp)^","
  in aux ""

let rec parse_types tl = parser
  | [< 'Ident "*/*"; ps = parse_params (1000,[]);
       r = more_types ((fst ps, Any (snd ps))::tl) >] -> r
  | [< 'Ident t; r = parse_type t tl >] -> r
and parse_type t tl = parser
  | [< 'Ident "/*"; ps = parse_params (1000,[]);
       r = more_types ((fst ps, AnySubtype (t, snd ps))::tl) >] -> r
  | [< 'Kwd "/"; 'Ident st; ps = parse_params (1000,[]);
       r = more_types ((fst ps, Type (t, st, snd ps))::tl) >] -> r
and parse_params (qv, pl) = parser
  | [< 'Kwd ";"; r = parse_param_or_q (qv, pl) >] -> r
  | [< >] -> (qv, List.rev pl)
and parse_param_or_q (qv, pl) = parser
  | [< 'Ident "q"; 'Kwd "="; ps = parse_q (qv, pl) >] -> ps
  | [< 'Ident p; 'Kwd "="; ps = parse_param p (qv, pl) >] -> ps
and parse_q (qv, pl) = parser
  | [< 'Int 1; ps = parse_params (1000,pl) >] -> ps
  | [< 'Int 0; ps = parse_params (0,pl) >] -> ps
  | [< 'Float q; ps = parse_params (q_of_float q,pl) >] -> ps
and parse_param p (qv, pl) = parser
  | [< 'Ident v; ps = parse_params (qv,(p, T v)::pl) >] -> ps
  | [< 'String v; ps = parse_params (qv,(p, S v)::pl) >] -> ps
and more_types tl = parser
  | [< 'Kwd ","; ts = parse_types tl >] -> ts
  | [< >] -> List.rev tl
