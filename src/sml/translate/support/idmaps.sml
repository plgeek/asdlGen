(* 
 *
 * COPYRIGHT (c) 1997, 1998, 1999 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature ID_MAP =
  sig
    val name : string
    val id_map  : (string * string) list
    val ty_map  : (string * string) list
  end

structure IdMaps =
    struct
	fun fix_kw s x = (x,x^s)

	structure Empty =
	  struct
	    val id_map = []
	    val ty_map = []
	    val name = "Empty"
	  end

 	structure SML =
	  struct
	    val id_map = ("","Ast")::(List.map (fix_kw "'")
	    ["and", "abstraction", "abstype", "as", "case",
	     "datatype", "else", "end", "eqtype", "exception", "do",
	     "fn", "fun", "functor", "funsig", "handle", "if", "in",
	     "include", "infix", "infixr", "let", "local", "nonfix", "o",
	     "of", "op", "open", "overload", "raise", "rec",
	     "ref","sharing", "sig", "signature", "struct", "structure",
	     "then", "type", "val", "where", "while", "with", "withtype",
	     "orelse", "andalso"])
	    
	    val ty_map = id_map
	    val name = "SML"
	  end
  
      structure Haskell =
	struct
	  val id_map = ("","Ast")::(List.map (fix_kw "'") 
	    ["case", "class", "data", "default", "deriving", "do",
	     "else", "if", "import", "in", "infix", "infixl",
	     "infixr", "instance", "let", "module", "newtype",
	     "of","then", "type", "where", "as" , "qualified",
	     "hiding"])
	  val ty_map = id_map
	  val name = "Haskell"
	end

      structure OCaml =
	struct
	  val id_map = ("","Ast")::(List.map (fix_kw "'")
	    ["and", "as", "assert", "asr", "begin", "class", "closed",
	     "constraint", "do", "done", "downto", "else", "end",
	     "exception", "external", "false", "for", "fun", "function",
	     "functor", "if", "in", "include", "inherit", "land", "lazy",
	     "let", "lor", "lsl", "lsr", "lxor", "match", "method", "mod",
	     "module", "mutable", "new", "of", "open", "or", "parser",
	     "private", "rec", "sig", "struct", "then", "to", "true", "try",
	     "type", "val", "virtual", "when", "while", "with"])
	  val ty_map = id_map
	  val name = "OCaml"
	end
      
       structure AnsiC =
	 struct
	   val id_map = (List.map (fix_kw "_")
	     ["auto", "break", "case", "char", "const",
	      "continue", "default", "do", "double", "else",
	      "enum", "extern", "float", "for", "goto", "if",
	      "int", "long", "register", "return", "short",
	      "signed", "sizeof", "static", "struct", "switch",
	      "typedef", "union", "unsigned", "void",
	      "volatile", "while"])
	   val ty_map = id_map@[("int","int")]
	   val name = "AnsiC"
	 end
	   
       structure CPlusPlus =
	 struct
	   val id_map = ("","Ast")::(List.map (fix_kw "_")
	     ["auto", "break", "case", "char", "const", "continue",
	      "default", "do", "double", "else", "enum", "extern", "float",
	      "for", "goto", "if", "int", "long", "register", "return",
	      "short", "signed", "sizeof", "static", "struct", "switch",
	      "typedef", "union", "unsigned", "void", "volatile", "while",
	      "catch", "class", "delete", "friend", "inline", "new",
	      "operator", "overload", "private", "protected", "public",
	      "template", "this", "try", "virtual","kind",
	      "typename" (* seems to be reserved by g++*)])
	   val ty_map = id_map@[("int","int")]
	   val name = "Cxx"
	 end

       structure Java =
	 struct
	   val id_map =
	     List.map (fix_kw "_")
	     ["abstract","boolean", "break", "byte", "case", "char",
	      "class", "const", "continue", "do", "double", "else",
	      "extends", "final","finally", "int", "float", "for",
	      "default", "if", "implements", "import", "instanceof",
	      "interface", "long", "native", "new", "public",
	      "short", "super", "switch", "synchroinized", "package",
	      "private", "protected", "transient", "return", "void",
	      "static", "while", "throw", "throws", "try",
	      "volatile","kind"]
	     
	   val ty_map = id_map 
	   val name = "Java"
	 end
       (* todo dump in the right keywords *)
       structure Icon =
	 struct
	   val id_map = List.map (fix_kw "_")
	     ["break", "do", "global", "next", "repeat", "to", "by",
	      "else", "if", "not", "return", "until", "case", "end",
	      "initial", "of", "static", "while", "create", "every",
	      "link", "procedure", "suspend", "default fail", "local",
	      "record", "then"]

	   val ty_map = id_map
	   val name = "Icon"
	 end
		   
    end