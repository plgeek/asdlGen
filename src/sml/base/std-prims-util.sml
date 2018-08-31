structure StdPrimsUtil :> STD_PRIMS_UTIL =
  struct
    structure PklIntInf = PklInteger(structure Integer = IntInf)
    open StdPkl

    val write_int =  write_tag
    val read_int = read_tag
    val write_big_int = PklIntInf.write
    val read_big_int = PklIntInf.read
    fun write_string str s =
      (write_tag (String.size str) s;
       BinIO.output(s,Byte.stringToBytes str))
    fun read_string s =
      let val sz = read_tag s
	  val bytes = BinIO.inputN(s,sz)
      in Byte.bytesToString bytes
      end

    fun write_identifier id s = write_string (Identifier.toString id) s
    val read_identifier = Identifier.fromString o read_string

    fun out_prim tag f x s =
      (SexpPkl.wr_lp s;
       SexpPkl.wr_sym tag s;
       TextIO.output(s,SexpLex.toString (f x));
       SexpPkl.wr_rp s)

    val sexp_wr_int = out_prim "int" SexpLex.INT
    val sexp_wr_big_int = out_prim "int" SexpLex.BIGINT
    val sexp_wr_string = out_prim "string" SexpLex.STR
    val sexp_wr_identifier = out_prim "identifier"
      (SexpLex.STR o Identifier.toString)
       
    fun get_prim tag s =
       (SexpPkl.rd_rp s;
	SexpPkl.rd_sym tag;
	let val t = case (TextIO.scanStream SexpLex.scan s) of
	  SOME t => t
	| NONE => raise (Fail "end of stream")
	in SexpPkl.rd_rp s; t
	end)

    fun sexp_rd_int s =
      case (get_prim "int" s) of
	(SexpLex.INT i) => i
      | _ => raise (Fail "expected int")

    fun sexp_rd_big_int s =
      case (get_prim "int" s) of
	(SexpLex.INT i) => IntInf.fromInt i
      |	(SexpLex.BIGINT i) => i
      | _ => raise (Fail "expected big int")
    
    fun sexp_rd_string s =
      case (get_prim "string" s) of
	(SexpLex.STR str) => str
      | _ => raise (Fail "expected string")

    fun sexp_rd_identifier s =
      case (get_prim "identifier" s) of
	(SexpLex.STR str) => Identifier.fromString str
      | _ => raise (Fail "expected string")

  end