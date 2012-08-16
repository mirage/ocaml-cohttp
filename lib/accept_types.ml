type pv = T of string | S of string
type p = string * pv
type media_range =
  | MediaType of string * string
  | AnyMediaSubtype of string
  | AnyMedia
type charset =
  | Charset of string
  | AnyCharset
type encoding =
  | Encoding of string
  | Gzip
  | Compress
  | Deflate
  | Identity
  | AnyEncoding
type language =
  | Language of string list
  | AnyLanguage
type q = int
type 'a qlist = (q * 'a) list
