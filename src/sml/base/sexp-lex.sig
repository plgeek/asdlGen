signature SEXP_LEX =
  sig
    datatype tok = 
      LP | RP  
    | INT of int
    | BIGINT of IntInf.int
    | SYM of string
    | STR of string
    val toString : tok -> string 
    val fromString : string -> tok option
    val scan : (char, 'a) StringCvt.reader -> (tok, 'a) StringCvt.reader
  end

