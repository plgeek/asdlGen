signature PP_DEPENDS =
  sig
    val makefile : (string list * string list list) list -> PPUtil.pp
    val cmfile   : (string list * string list list) list -> PPUtil.pp
  end

structure PPDepends :> PP_DEPENDS =
  struct
    open PPUtil
    fun pp_unix_path x = seq {fmt=s,sep=s "/"} x
    fun pp_dep (lhs,rhs) =
      cat [pp_unix_path lhs,s " : ", vbox 4 [seq {fmt=pp_unix_path,
						sep=cat [s " \\",nl]} rhs]]

      
    fun makefile args = seq_term {fmt=pp_dep,sep=nl} 
      (List.filter (not o List.null o (fn (_,x) => x)) args)

    fun cmfile args =
      vbox 2 [s "Group is",nl,
	      seq {fmt=(pp_unix_path o #1), sep=nl}
	      (List.filter (not o List.null o (fn (_,x) => x)) args)]
	   
  end