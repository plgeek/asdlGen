signature CVT_BITS =
    sig
	type hexdigits = string

	val bits2string : hexdigits -> string
    end

structure CvtBits :> CVT_BITS =
    struct
	type hexdigits = string

	fun  bits2string  d =
	    let
		fun char2hex #"0" = 0w0
		  | char2hex #"1" = 0w1
		  | char2hex #"2" = 0w2
		  | char2hex #"3" = 0w3
		  | char2hex #"4" = 0w4
		  | char2hex #"5" = 0w5
		  | char2hex #"6" = 0w6
		  | char2hex #"7" = 0w7
		  | char2hex #"8" = 0w8
		  | char2hex #"9" = 0w9
		  | char2hex #"a" = 0w10
		  | char2hex #"b" = 0w11 
		  | char2hex #"c" = 0w12
		  | char2hex #"d" = 0w13
		  | char2hex #"e" =  0w14
		  | char2hex #"f" =  0w15
		  | char2hex _ = raise (Fail "bad character")
		fun get_byte(i) =
		    let val j = 2 * i;
			val hi = (char2hex(String.sub(d,j)))
			val lo = (char2hex(String.sub(d,j+1)))
		    in
			Word8.+(Word8.<<(hi,0w4),lo)
		    end
		val _ = if (String.size(d) mod 2) = 1 then
		    raise (Fail "bad string length")
			else ()
		val vec =
		    Word8Vector.tabulate(String.size(d) div 2,get_byte)
	    in
		Byte.bytesToString(vec)
	    end
    end