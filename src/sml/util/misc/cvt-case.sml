signature CVT_CASE =
  sig
    type cvt 
    val toUpper      : cvt
    val toLower      : cvt


    val capitalize   : cvt
    val uncapitalize : cvt

    val compose      : cvt -> cvt -> cvt
    val map          : (string -> string) -> cvt

    val cvt_string   : cvt -> string -> string
    (* apply cvt to every word where a word begins
       with a sequence of capital letters *)
    val cvt_cap_words : cvt -> string -> string
    (* apply cvt to every word seperated by an "_" *)
    val cvt_sep_char: cvt -> char -> string -> string
  end

structure CvtCase :> CVT_CASE =
  struct
    type cvt = (string -> string)

    val toUpper = String.map Char.toUpper
    val toLower = String.map Char.toLower

    fun capitalize "" = ""
      | capitalize s =
      let val size = String.size s
	val first = Char.toUpper(String.sub(s,0))
	val rest = if size > 1 then String.extract(s,1,NONE) else ""
      in (String.str first)^rest
      end

    fun uncapitalize "" = ""
      | uncapitalize s =
      let val size = String.size s
	val first = Char.toLower(String.sub(s,0))
	val rest = if size > 1 then String.extract(s,1,NONE) else ""
      in (String.str first)^rest
      end
    fun cvt_string cvt = cvt 
    fun cvt_sep_char cvt c s = 
      let val fields = String.fields (fn x => x = c) s
      in ListFormat.fmt{init="",final="",sep=String.str c,fmt=cvt} fields
      end
   fun compose x y = x o y
   fun map f = f
      
    fun cvt_cap_words cvt s = 
      let val subs = Substring.all s
	fun go (c,(word,words)) =
	  case word of
	    [] => ([c],words)
	  | x::xs => 
	      if (Char.isLower c) andalso (Char.isUpper x) then
		([c],(String.implode word)::words)
	      else (c::word,words)
	val (word,words) = Substring.foldr go ([],[]) subs
	val words = (String.implode word)::words
	val words = List.map cvt words
      in String.concat words
      end
  end
  