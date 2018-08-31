(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(* SML/NJ specific *)
signature STD_TYPES = 
    sig
	type nat = Int.int
	type int8 = Word8.word
	type ieee_real = Real.real
	type int16 = Int.int
	type int32 = Int32.int
	type int64 = IntInf.int
	type uint8 = Word8.word
	type uint16 = Word.word
	type uint32 = Word32.word
	type uint64 = IntInf.int
        type bool = Bool.bool
    end
signature STD_TYPES_UTIL =
  sig
    include STD_TYPES
    type 'a std_wr = 'a -> StdPkl.outstream -> unit
    type 'a std_rd = StdPkl.instream -> 'a

    type 'a sexp_wr = 'a -> SexpPkl.outstream -> unit
    type 'a sexp_rd = SexpPkl.instream -> 'a

    val write_nat    : nat std_wr
    val write_bool   : bool std_wr

    val write_int8   : int8 std_wr
    val write_int16  : int16 std_wr
    val write_int32  : int32 std_wr
    val write_int64  : int64 std_wr

    val write_uint8  : uint8 std_wr
    val write_uint16 : uint16 std_wr
    val write_uint32 : uint32 std_wr
    val write_uint64 : uint64 std_wr

    val read_bool    : bool std_rd
    val read_nat     : nat std_rd

    val read_int8    : int8 std_rd
    val read_int16   : int16 std_rd
    val read_int32   : int32 std_rd
    val read_int64   : int64 std_rd

    val read_uint8   : uint8 std_rd
    val read_uint16  : uint16 std_rd
    val read_uint32  : uint32 std_rd
    val read_uint64  : uint64 std_rd

    val sexp_wr_nat    : nat sexp_wr
    val sexp_wr_bool   : bool sexp_wr

    val sexp_wr_int8   : int8 sexp_wr
    val sexp_wr_int16  : int16 sexp_wr
    val sexp_wr_int32  : int32 sexp_wr
    val sexp_wr_int64  : int64 sexp_wr

    val sexp_wr_uint8  : uint8 sexp_wr
    val sexp_wr_uint16 : uint16 sexp_wr
    val sexp_wr_uint32 : uint32 sexp_wr
    val sexp_wr_uint64 : uint64 sexp_wr

    val sexp_rd_bool    : bool sexp_rd
    val sexp_rd_nat     : nat sexp_rd

    val sexp_rd_int8    : int8 sexp_rd
    val sexp_rd_int16   : int16 sexp_rd
    val sexp_rd_int32   : int32 sexp_rd
    val sexp_rd_int64   : int64 sexp_rd

    val sexp_rd_uint8   : uint8 sexp_rd
    val sexp_rd_uint16  : uint16 sexp_rd
    val sexp_rd_uint32  : uint32 sexp_rd
    val sexp_rd_uint64  : uint64 sexp_rd


(*  unimplemented
    val sexp_wr_ieee_real : ieee_real std_wr
    val read_ieee_real  : ieee_real std_rd
*)
  end

structure StdTypes :> STD_TYPES =
  struct
    type nat = Int.int
    type int8 = Word8.word
    type ieee_real = Real.real
    type int16 = Int.int
    type int32 = Int32.int
    type int64 = IntInf.int
    type uint8 = Word8.word
    type uint16 = Word.word
    type uint32 = Word32.word
    type uint64 = IntInf.int
    type bool = Bool.bool
  end

structure StdTypesUtil :> STD_TYPES_UTIL =
    struct
      type 'a std_wr = 'a -> StdPkl.outstream -> unit
      type 'a std_rd = StdPkl.instream -> 'a
	
      type 'a sexp_wr = 'a -> SexpPkl.outstream -> unit
      type 'a sexp_rd = SexpPkl.instream -> 'a
      open StdTypes

      structure PklInt   = PklInteger(structure Integer = Int)
      structure PklInt32 = PklInteger(structure Integer = Int32)
	
      structure PklWord   = PklWord(structure Word = Word)
      structure PklWord8   = PklWord(structure Word = Word8)
      structure PklWord32 = PklWord(structure Word = Word32)
	
      structure PklIntInf = PklInteger(structure Integer = IntInf)
      open StdTypes
	
      fun write_bool true = PklInt.write 2
	| write_bool false = PklInt.write 1
	
      val write_nat = PklInt.write
      val write_int8 = PklWord8.write
      val write_int16 = PklInt.write
      val write_int32 = PklInt32.write
      val write_int64 = PklIntInf.write
	
      val write_uint8  = PklWord8.write
      val write_uint16 = PklWord.write
      val write_uint32 = PklWord32.write
      val write_uint64 = PklIntInf.write
      fun write_ieee_real _ _ = raise (Fail "write_ieee_real unimplemented")
	
      fun read_bool s =
	(case (PklInt.read s) of
	   2 => false
	 | 1 => true
	 | _ => raise (StdPkl.IOError "read_bool"))
	   
      val read_nat = PklInt.read
      val read_int8 = PklWord8.read
      val read_int16 = PklInt.read
      val read_int32 = PklInt32.read
      val read_int64 = PklIntInf.read
	
      val read_uint8  = PklWord8.read
      val read_uint16 = PklWord.read
      val read_uint32 = PklWord32.read
      val read_uint64 = PklIntInf.read
      fun read_ieee_real _ = raise (Fail "read_ieee_real unimplemented")
	
      fun out_value tag f x s =
	(SexpPkl.wr_lp s;
	 SexpPkl.wr_sym ("StdTypes_"^tag) s;
	 TextIO.output(s,SexpLex.toString (f x));
	 SexpPkl.wr_rp s)
	

      val sexp_wr_nat = out_value "nat" SexpLex.INT
      fun sexp_wr_bool true s = 
	(SexpPkl.wr_lp s;
	 SexpPkl.wr_sym ("StdTypes_TRUE") s;
	 SexpPkl.wr_rp s)
	| sexp_wr_bool false s =
	(SexpPkl.wr_lp s;
	 SexpPkl.wr_sym ("StdTypes_FALSE") s;
	 SexpPkl.wr_rp s)

      val sexp_wr_int8 = out_value "int8" (SexpLex.INT o Word8.toInt)
      val sexp_wr_int16 = out_value "int16" SexpLex.INT
      val sexp_wr_int32 = out_value "int32" (SexpLex.BIGINT o IntInf.fromLarge)
      val sexp_wr_int64 = out_value "int64"  SexpLex.BIGINT

      val sexp_wr_uint8 = out_value "uint8" (SexpLex.INT o Word8.toInt)
      val sexp_wr_uint16 = out_value "uint16" (SexpLex.INT o Word.toInt)
      val sexp_wr_uint32 = out_value "uint32" 
	(SexpLex.BIGINT o IntInf.fromLarge o Word32.toLargeInt)
      val sexp_wr_uint64 = out_value "uint64" SexpLex.BIGINT

      fun get_value tag s =
	(SexpPkl.rd_rp s;
	SexpPkl.rd_sym ("StdTypes_"^tag);
	 let val t = case (TextIO.scanStream SexpLex.scan s) of
	   SOME t => t
	 | NONE => raise (Fail "end of stream")
	 in SexpPkl.rd_rp s; t
	 end)

      fun toInt (SexpLex.INT i) = i
	| toInt (SexpLex.BIGINT i) = IntInf.toInt i
	| toInt _ = raise (Fail "bad token")

      fun toLarge (SexpLex.INT i) = Int.toLarge i
	| toLarge (SexpLex.BIGINT i) = IntInf.toLarge i
	| toLarge _ = raise (Fail "bad token")

      fun toBig (SexpLex.INT i) = IntInf.fromInt i
	| toBig (SexpLex.BIGINT i) = i
	| toBig _ = raise (Fail "bad token")

      val toWord8 = Word8.fromLargeInt o toLarge
      val toWord = Word.fromLargeInt o toLarge
      val toWord32 = Word32.fromLargeInt o toLarge

      val sexp_rd_nat = toInt o (get_value "nat")
      fun sexp_rd_bool s = 
	(SexpPkl.rd_lp s;
	 let val v = (case SexpPkl.get_sym s of
			"StdTypes_TRUE" => true
		      | "StdTypes_FALSE" => false
		      |  _ => raise (Fail "bad token"))
	 in SexpPkl.rd_rp s;v
	 end)

      val sexp_rd_int8 = toWord8 o (get_value "int8")
      val sexp_rd_int16 = toInt o (get_value "int16")
      val sexp_rd_int32 = toLarge o (get_value "int32")
      val sexp_rd_int64 = toBig o (get_value "int64")

      val sexp_rd_uint8  = toWord8 o (get_value "uint8")
      val sexp_rd_uint16 = toWord o (get_value "uint16")
      val sexp_rd_uint32 = toWord32 o (get_value "uint32")
      val sexp_rd_uint64 = toBig o (get_value "uint64")
    end
