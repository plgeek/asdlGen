(**
\section*{Parsing Input Files}
This code should hide all the language specific parsing details.
**)(*::*)
(*:interface to [[structure DTangleLexer]]:*)
signature DTANGLE_LEXER =
  sig
(**
 All tokens have an associated position which indicates the character
 position where the token starts.
**)
    (*:definition of the tokens:*)
    datatype toks =
      NodeBegin  of {pos:int,name:string,doc:string list}
    | Line       of {pos:int,line:string}
    | NodeEnd    of {pos:int}
    | EOF        of {pos:int}
    (**)

    (*:abstract type that defines concrete tokens:*)
    type token_spec
    (**)
    (*:predefined [[token_spec]]s:*)
    val ml_spec    : token_spec
    val c_spec     : token_spec
    val shell_spec : token_spec
    val ada_spec   : token_spec
    val lisp_spec  : token_spec
    val null_spec  : token_spec
    val tex_spec   : token_spec
    (**)
(**
 The [[mkLexer]] function takes a [[token_spec]] and input stream plus
 the initial value to use as the position of the first character in
 the stream.
**)      
    (*:main entry point:*)
    val mkLexer    : {spec:token_spec,
		       ins:TextIO.instream,
		       pos:int} -> unit -> toks
    (**)
  end
(**)
(**
This implementation uses the [[structure ParserComb]] structure from
the SML/NJ library. The lexer produced by this structure therefore
may backtrack, which is easy to do with functional stream IO. Ambitious 
hackers can rewrite this as serveral ML-Lex structures.
**)
(*:implementation using [[structure ParserComb]]:*)
structure DTangleLexer :> DTANGLE_LEXER =
  struct
    structure P = ParserComb
    (*:types and misc definitions:*)
    datatype toks =
      NodeBegin  of {pos:int,name:string,doc:string list}
    | Line       of {pos:int,line:string}
    | NodeEnd    of {pos:int}
    | EOF        of {pos:int}
    type tok = (string,(TextIO.StreamIO.instream * int)) P.parser
    type token_spec =
      {doc_begin     : tok,
       doc_cleaner   : Char.char -> bool,
       doc_end       : tok,
       code_sep_begin: tok,
       code_sep_end  : tok,
       node_end      : tok}
    (*:concrete [[token_spec]]:*)
    val ml_spec : token_spec =
      {doc_begin=P.string "(**",
       doc_cleaner=(fn _ => false),
       doc_end=P.skipBefore Char.isSpace (P.string "**)"),
       code_sep_begin= P.string "(*:",
       code_sep_end=P.string ":*)",
       node_end=P.string "(**)"}
    (**)
    (*:concrete [[token_spec]]:*)
    val c_spec : token_spec =
      {doc_begin=P.string "/**",
       doc_cleaner=(fn _ => false),
       doc_end=P.skipBefore Char.isSpace (P.string "**/"),
       code_sep_begin=P.string "/*:",
       code_sep_end=P.string ":*/",
       node_end=P.string "/**/"}
    (**)
    (*:concrete [[token_spec]]:*)
    val shell_spec : token_spec =
      {doc_begin=P.string "#*",
       doc_cleaner=(fn #"#" => true | _ => false),
       doc_end=P.string "#*",
       code_sep_begin=P.string "#:",
       code_sep_end=P.string ":",
       node_end=P.string "##"}
    (**)
    (*:concrete [[token_spec]]:*)
    val ada_spec : token_spec =
      {doc_begin=P.string "--*",
       doc_cleaner=(fn #"-" => true | _ => false),
       doc_end=P.string "--*",
       code_sep_begin=P.string "--:",
       code_sep_end=P.string ":",
       node_end=P.string "----"}
    (**)
    (*:concrete [[token_spec]]:*)
    val lisp_spec : token_spec =
      {doc_begin=P.string ";*",
       doc_cleaner=(fn #"-" => true | _ => false),
       doc_end=P.string ";*",
       code_sep_begin=P.string ";:",
       code_sep_end=P.string ":",
       node_end=P.string ";;"}
    (**)
    (*:concrete [[token_spec]]:*)
    val tex_spec : token_spec =
      {doc_begin=P.string "%*",
       doc_cleaner=(fn #"%" => true | _ => false),
       doc_end=P.string "%*",
       code_sep_begin=P.string "%:",
       code_sep_end=P.string ":",
       node_end=P.string "%%"}
    (**)
    (*:concrete [[token_spec]]:*)
    val null_spec : token_spec =
      {doc_begin=P.failure,
       doc_cleaner=(fn  _ => false),
       doc_end=P.failure,
       code_sep_begin=P.failure,
       code_sep_end=P.failure,
       node_end=P.failure}
    (**)
    (**)
(**
 The parsers consume functional streams based on the
 [[structure TextIO.StreamIO]] functional character streams.
 Internally the streams are augmented with a character position to
 keep track of where tokens are defined.
**) 
    (*:internal streams:*)
    type stream = (TextIO.StreamIO.instream * int)
    (**)
(**
 The [[scanStream]] function convernts and imperative
 [[TextIO.instream]] function to our internal functionall
 representations and passes the result to the [[scanFn]]. If the
 [[scanFn]] is successful the imperative [[TextIO.instream]] is
 updated. The result is the scaned token plus the new position of the stream.
 **)
    (*:internal streams:*)
    fun scanStream scanFn (strm,p) = let
      val instrm = TextIO.getInstream strm
      fun getc (s,p) =
	case(TextIO.StreamIO.input1 s) of
	  NONE => NONE
	| (SOME(c,s)) => SOME(c,(s,p+1))
    in
      case (scanFn getc (instrm,p))
	of NONE => (NONE,p)
      | SOME(v, (instrm',p)) =>
	  (TextIO.setInstream (strm, instrm');
	   (SOME v,p))
    end
    (**)
(**
 Here are a few special stream scanners that ignore the normal
 [[getc]] argument and use the [[TextIO.StreamIO]] primitives instead.
 **)
    (*:special stream scanners:*)
    fun eof  _ (s,p) =
      if TextIO.StreamIO.endOfStream s then
	SOME ((),(s,p))
      else NONE
	
    fun line _ (s,p) =
      (case (TextIO.StreamIO.inputLine s) of
	 ("",_) => NONE
       | (x,s') => (SOME (x,(s',p+(String.size x)))))
    (**)
(**
 The [[any]] combinator matches and returns any character.
 **)	 
    (*:speical parser combinators:*)
    fun any getc = getc
    (* include because 110.3 version of bind is broken *)
    fun bind (p1, p2') getc strm =
      (case (p1 getc strm)
	 of SOME(t1, strm1) => p2' t1 getc strm1
       | NONE => NONE)
    (**)
(** 
 The [[mark_pos]] combinator returns the token matched plus the start
 position of the token.
 **)
    (*:speical parser combinators:*)
    fun mark_pos f getc = let
      fun rd (strm as (_,tok_start)) =
	(case ((f getc) strm) of
	   NONE => NONE
	 | SOME (x,rest) => SOME((tok_start,x),rest))
    in
      rd
    end
    (**)      
(** 
 The [[delim]] combinator takes three tokens. The first matches the
 start of a sequence of tokens described by the second argument.
 The sequence is terminated by the ending token which is the last argument.
 **)
    (*:speical parser combinators:*)
    fun delim (s,elt,e) = let
      fun parse xs (SOME e) = P.result (List.rev xs,e)
	 | parse xs NONE =
	bind (elt,(fn x => bind (P.option e,parse (x::xs))))
    in
      P.seqWith (fn (x,(y,z)) => (x,y,z)) (s,bind(P.option e,parse[]))
    end
    (**)        
(**
 The [[eatws]] combinator takes a token and converts it into a new
 token where the can be an arbitrary amount of whitespace infront of
 the original token which is ignored. The [[kill_sp]] combinator trims
 any whitespace up to and including the first newline.
 **)
    (*:speical parser combinators:*)
    fun eatws p = P.skipBefore Char.isSpace  p
    fun kill_sp p = let
      fun is_sp #"\n" = false | is_sp x = (Char.isSpace x)
    in
      P.seqWith #1
      (p,P.seq(P.option (P.token is_sp),
	       P.option (P.char #"\n")))
    end
    (**)
    fun mk_doc_start doc_start =
      let val doc_arg =
	P.option (P.wrap (delim(P.char #":",any,P.char #":"),
			  (fn (x,y,z) => (String.implode y))))
	  fun get_opt (_,SOME x) = SOME x
	    | get_opt (_,NONE ) = NONE
      in
	P.seqWith get_opt (doc_start,doc_arg)
      end

    fun mkLexer {spec,ins,pos} = let
      (*:convert [[token_spec]] into tokens:*)
      val {doc_begin, doc_cleaner, doc_end,
	   code_sep_begin, code_sep_end,
	   node_end} = spec

      val doc_begin_tok       = (kill_sp o mark_pos) (mk_doc_start doc_begin)
      val doc_end_tok         = (kill_sp o mark_pos) doc_end
      val code_sep_begin_tok  = (eatws o mark_pos)   code_sep_begin
      val code_sep_end_tok    = (kill_sp o mark_pos) code_sep_end
      val node_end_tok        = (kill_sp o eatws o mark_pos) node_end
	
      val doc_tok =
	delim (doc_begin_tok,
	       P.skipBefore doc_cleaner line,
	       doc_end_tok)
	
      val code_tok =
	P.wrap
	(delim (code_sep_begin_tok,any,code_sep_end_tok),
	 (fn (x,y,z) => (x,String.implode y,z)))

      val node_begin_tok =
	let
	  fun mk_begin d ((p,_),n,_) = {pos=p,doc=d,name=n}
	  fun get_begin NONE = P.wrap (code_tok,mk_begin [])
	    | get_begin (SOME ((p,SOME n),d,_)) =
	    P.result ({pos=p,doc=d,name=n})
	    | get_begin (SOME ((p,NONE),doc,_)) =
	    P.wrap (code_tok,(mk_begin doc))
	in
	  bind (P.option doc_tok,get_begin)
	end

      val line_tok = mark_pos line
      val eof_tok = mark_pos eof
      (**)
      (*:construct return values:*)
      fun mkNodeEnd (p,s) = NodeEnd {pos=p}
      fun mkLine    (p,l) = Line{pos=p,line=l}
      fun mkEOF     (p,_) = EOF{pos=p}
      (**)
      val tokens =
	P.or' [P.wrap (node_end_tok,mkNodeEnd),
	       P.wrap (node_begin_tok,NodeBegin),
	       P.wrap (line_tok,mkLine),
	       P.wrap (eof_tok,mkEOF)]
(**
Notice the use of a ref to keep track of the current position.
**)
      (*:return function to get the next token:*)
      val pos = ref pos
      fun gettok () =
	(case (scanStream tokens (ins,(!pos))) of
	   (NONE,p) => raise (Fail "lexer error")
	 | (SOME x,p) => (pos:=p;x))
       (**)
    in
      gettok
    end
  end
  (**)
(**)