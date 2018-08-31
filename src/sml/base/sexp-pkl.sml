structure SexpPkl :> SEXP_PKL  =
  struct
    fun die () = raise (Fail "Sexp Pickler Error")
    type instream = TextIO.instream
    type outstream = TextIO.outstream
    fun out_tok t s = TextIO.output(s,SexpLex.toString t)

    val wr_lp = out_tok SexpLex.LP
    val wr_rp = out_tok SexpLex.RP
    fun wr_sym sym = out_tok (SexpLex.SYM sym)
    fun wr_list f l s =
      let
	fun loop [] = ()
	  | loop (x::xs) = (f x s;loop xs)
      in (wr_lp s;wr_sym "list" s;loop l;wr_rp s)
      end
    fun wr_option f NONE s = wr_list f [] s
      | wr_option f (SOME v) s = wr_list f [v] s

    fun expect_tok t s =
      (case (TextIO.scanStream SexpLex.scan s) of
	 SOME t' => if (t = t') then ()
		    else raise (Fail ("expected:"^(SexpLex.toString t)^
				      "read:"^(SexpLex.toString t')))
       | NONE =>  raise (Fail "bad token"))
    val rd_lp = expect_tok SexpLex.LP
    val rd_rp = expect_tok SexpLex.RP


    fun peekStream scanFn strm =
      let val instrm = TextIO.getInstream strm
      in case (scanFn TextIO.StreamIO.input1 instrm)
	of NONE => NONE
      | SOME(v, instrm') => SOME v
      end

    fun get_sym s =
      (case (TextIO.scanStream SexpLex.scan s) of
	 SOME(SexpLex.SYM str) => str
       | _ => raise (Fail "error reading symbol"))

    fun rd_sym str s = if (get_sym s) = str then ()
                       else raise (Fail ("expected:"^str))
    fun rd_list f s =
      let
	fun peek_rp () = 
	  (case (peekStream SexpLex.scan s) of
	     SOME SexpLex.RP => true
	   | _ => false)
	fun loop false =
	  let val x = (f s)
	  in x::(loop (peek_rp ()))
	  end
	  | loop true = (rd_rp s;[])
      in rd_lp s;rd_sym "list" s;loop (peek_rp())
      end

    fun rd_option f s =
      case (rd_list f s) of
	[] => NONE
      | [x] => (SOME x)
      | (x::xs) => raise (Fail "bad option type")

    fun rd_share _ _  = raise Fail "Unimplemented"
    fun wr_share _ _ _ = raise Fail "Unimplemented"
  end 


