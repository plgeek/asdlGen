signature COMMAND_OPTIONS =
  sig
    (* command line options *)

    type 'a flag_desc = {name:string,
		       flags:string,
			   v:'a,
			dflt:'a,
			 doc:string}
    type 'a param_desc = {name:string,
		       flags:string,
		    arg_dflt:'a option,
			dflt:'a,
		      advice:string,
			 doc:string}
    type args
    type args_spec
    type 'a flag = 'a flag_desc -> (args_spec * (args -> 'a)) 
    type 'a param = 'a param_desc  -> (args_spec * (args -> 'a)) 

    val empty      : args_spec

    val boolFlag    : args_spec -> bool flag
    val intFlag     : args_spec -> int flag
    val stringFlag  : args_spec -> string flag

    val boolParam   : args_spec -> bool param
    val intParam    : args_spec -> int param
    val stringParam : args_spec -> string param

    val merge      : (args_spec * args_spec) -> args_spec
    val layer      : (args_spec * args_spec) -> args_spec

    val getRest : args -> string list
    val mkUsage : args_spec -> string -> string
    val mkCmd   : args_spec -> (args -> 'a) ->
                   (string * string list) -> 'a option
    val toArgList : args -> string list
  end