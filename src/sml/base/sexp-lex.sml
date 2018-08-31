structure SexpLex :> SEXP_LEX =
  struct
    datatype tok =
      LP | RP  
    | INT of int
    | BIGINT of IntInf.int
    | SYM of string
    | STR of string

    fun toString LP = "("
      | toString RP = ")"
      | toString (INT i) =
      if i < 0 then "-"^(Int.toString (~i))
      else Int.toString i
      | toString (BIGINT i) =
      if (IntInf.sign i) < 0 then "-"^(IntInf.toString (IntInf.~(i)))
      else IntInf.toString i
      | toString (SYM s) = s^" "
      | toString (STR s) = "\""^(String.toCString s)^"\""
      
    fun scan (getc:(char,'a) StringCvt.reader) =
      let
	fun eatws s =
	  case (getc s) of
	    SOME (c,s') => if (Char.isSpace c) then eatws s'
			   else s
	  | NONE => s
	fun scan_sym s =
	  let
	    fun loop (s,str) =
	      case (getc s) of
		SOME (c,s') =>
		  if ((Char.isAlphaNum c) orelse c = #"_") then
		    loop(s',c::str)
		  else (String.implode (List.rev str),s)
	      | NONE => (String.implode (List.rev str),s)
	  in case (getc s) of
	    SOME (c,s) =>
	      if (Char.isAlpha c) then SOME (loop(s,[c]))
	      else NONE
	  | _ => NONE
	  end
(*	
 The allowable escape sequences are given below
 (cf. Section 6.1.3.4 of the ISO C standard ISO/IEC
 [CITE]9899:1990/). 
 
	       \a       Alert (ASCII 0x07)
               \b       Backspace (ASCII 0x08)
               \t       Horizontal tab (ASCII 0x09)
               \n       Linefeed or newline (ASCII 0x0A)
               \v       Vertical tab (ASCII 0x0B)
               \f       Form feed (ASCII 0x0C)
               \r       Carriage return (ASCII 0x0D)
               \?       Question mark
               \\       Backslash
               \"       Double quote
               \'       Single quote
               \^c      A control character whose encoding is C - 64, where C
                        is the encoding of the character c, with C in the range
                        [64,95].
               \ddd     The character whose encoding is the number ddd, where
                        ddd consists of one to three octal.
               \uxxxx   The character whose encoding is the number xxxx, 
                        where xxxx is a sequence of hexadecimal digits.
 *)              
	(* scan the rest of a string *)
	fun scan_str s =
	  let
	    fun do_escape s =
	      case (getc s) of
		SOME (#"a",s) => (Char.chr 0x07,s)
	      | SOME (#"b",s) => (Char.chr 0x08,s)
	      | SOME (#"t",s) => (Char.chr 0x09,s)
	      | SOME (#"n",s) => (Char.chr 0x0A,s)
	      | SOME (#"v",s) => (Char.chr 0x0B,s)
	      | SOME (#"f",s) => (Char.chr 0x0C,s)
	      | SOME (#"r",s) => (Char.chr 0x0D,s)
	      | SOME (#"?",s) => (#"?",s)
	      | SOME (#"\\",s) => (#"\\",s)
	      | SOME (#"\"",s) => (#"\"",s)
	      | SOME (#"'",s) => (#"'",s)
	      | SOME (#"^",s) =>
		  (case (getc s) of
		    SOME (c,s') =>
		      if (Char.ord c) > 63 andalso (Char.ord c) < 96 then
			(Char.chr ((Char.ord c) - 64),s')
		      else (#"^",s)
		  | NONE => (#"^",s))
	      | SOME (#"u",s) => 
		     (case (Int.scan StringCvt.HEX getc s) of
		       SOME (i,s) => (Char.chr i,s)
		     | NONE => (#"u",s))
	      | SOME (c,s) => 
		     (case (Int.scan StringCvt.OCT getc s) of
		       SOME (i,s) => (Char.chr i,s)
		     | NONE => (c,s))
	      | NONE => raise (Fail "Unfinished Escape")

	    fun loop(s,acc) =
	      case (getc s) of
		SOME (#"\\",s) =>
		  let val (c,s) = do_escape s
		  in loop(s,c::acc)
		  end
	      | SOME (#"\"",s) => SOME(String.implode (List.rev acc),s)
	      | SOME (c,s) => loop(s,c::acc)
	      | NONE => raise (Fail "Unclosed String")
	  in loop(s,[])
	  end
	fun scan_int s = Int.scan StringCvt.DEC getc s
	fun scan_bigint s = IntInf.scan StringCvt.DEC getc s
	fun scan_tok s =
	    let val s = eatws s
	    in case (getc s) of
		      SOME (#"(",s) => SOME(LP,s)
		    | SOME (#")",s) => SOME(RP,s)
		    | SOME (#"\"",s) =>
			    (case (scan_str s) of
			       SOME(str,s) => SOME(STR str,s)
			     | NONE => NONE)
		    | SOME (_,_) =>
			((case scan_int s of
		           SOME(i,s) => SOME(INT i,s)
			 | NONE => (case (scan_sym s) of
				      SOME(str,s) => (SOME(SYM str,s))
				    | NONE => NONE))
			    handle Overflow =>
			      (case scan_bigint s of
				 SOME (i,s) => SOME(BIGINT i,s)
			       | NONE => NONE))
		    |  NONE => NONE
	    end
      in scan_tok
      end
    val fromString = StringCvt.scanString scan
  end
