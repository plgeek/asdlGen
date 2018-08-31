(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
functor PklInteger(structure Integer : INTEGER) : PKL =
    struct
	structure IO = BinIO
	structure W8 = Word8
	structure W8V = Word8Vector

	type T = Integer.int

	val c_0 = Integer.fromInt 0
	val c_1 = Integer.fromInt 1
	val c_128 = Integer.fromInt 128
	val c_63 = Integer.fromInt 63
	    
	val w8toi = Integer.fromLarge o W8.toLargeInt
	val itow8 = W8.fromLargeInt o Integer.toLarge
	    
	fun write x s =
	    let
		val sign = Integer.sign(x)
		fun set_continue x = W8.orb(x,0wx80)
		fun set_neg x = W8.orb(x,0wx40)
		fun nibble x = (itow8 (Integer.rem(x,c_128)))
		fun rest x = (itow8 (Integer.rem(x,c_128)))
		fun finish (~1) x = (set_neg (itow8 x))
		  | finish _ x  = itow8 x
		fun loop (x) =
		    if Integer.>(x,c_63) then
			(IO.output1(s,set_continue(nibble x));
			loop(Integer.quot(x,c_128)))
		    else IO.output1(s,finish sign x)

		(* take care of 2's complement asymetry*)
		val x =
		    (Integer.abs(x) handle
		     Overflow =>
			 (IO.output1
			  (s,set_continue(itow8 (Integer.mod(x,c_128))));
			  Integer.abs(Integer.div(x,c_128))))
	    in loop (x)
	    end

	fun read s =
	    let
		fun is_continue_bit_set (x) =
		    not(W8.andb(x,0wx80) = 0wx0)
		fun is_neg_bit_set (x) =  not(W8.andb(x,0wx40) = 0wx0)
		fun mask_neg (x) =  (W8.andb(x,0wx3f))
		fun nibble x = (w8toi(W8.andb(x,0wx7f)))
		fun loop(x,acc,coef) =
		    if(is_continue_bit_set x) then
			loop(Option.valOf (IO.input1 s),
			     Integer.+(acc,Integer.*(nibble x,coef)),
			     Integer.*(coef,c_128))
		    else
			let
			    val v = w8toi (mask_neg x)
			    val last = Integer.*(v,coef)
			in
			    (* take care of 2's complement asymetry*)
			    if (is_neg_bit_set x) then
				Integer.-(Integer.~(acc),last)
			    else
				Integer.+(acc,last)
			end			    
		val ret = loop(Option.valOf(IO.input1 s),c_0,c_1)
	    in
		ret
	    end	
    end


functor PklWord(structure Word : WORD) : PKL =
    struct
	structure IO = BinIO
	structure W8 = Word8
	structure W8V = Word8Vector

	type T = Word.word

	val c_0 = Word.fromInt 0
	val c_1 = Word.fromInt 1
	val c_128 = Word.fromInt 128
	val c_63 = Word.fromInt 63
	    
	val w8tow = Word.fromLargeWord o W8.toLargeWord
	val wtow8 = W8.fromLargeWord o Word.toLargeWord
	    
	fun write x s =
	    let
		fun set_continue x = W8.orb(x,0wx80)
		fun loop x =
		    if Word.>(x,c_63) then
			(IO.output1(s,set_continue(wtow8 x));
			loop(Word.>>(x,0w7)))
		    else IO.output1(s,wtow8 x)
	    in
		loop x
	    end

	fun read s =
	    let
		fun is_continue_bit_set (x) =
		    not(W8.andb(x,0wx80) = 0wx0)
		fun is_neg_bit_set (x) =  not(W8.andb(x,0wx40) = 0wx0)
		fun nibble x = (w8tow(W8.andb(x,0wx7f)))
		fun mask_neg (x) =  (W8.andb(x,0wx3f))
		fun loop(x,acc,shift) =
		    if(is_continue_bit_set x) then
			loop(Option.valOf (IO.input1 s),
			     Word.orb(acc,Word.<<(nibble x,shift)),
			     shift+0w7)
		    else
			let
			    val v = w8tow (mask_neg x)
			    val acc =
				Word.orb(acc,Word.<<(v,shift))
			in
			    if (is_neg_bit_set x) then
				(TextIO.output(TextIO.stdErr,
				"Ignoring sign bit on unsigned read\n");
				 acc)
			    else
				acc
			end			    
		val ret = loop(Option.valOf(IO.input1 s),c_0,0w0)
	    in
		ret
	    end	
    end

