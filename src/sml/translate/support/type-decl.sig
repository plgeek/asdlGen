(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The [[TYPE_DECL]] signature provides an abstract interface for
generating code for a particular target language. It is used by the
pickler generators to produce code for pickling ASDL values. It
describes an environment ([[env]]) of types that contain type
identifiers ([[ty_id]]) bound to descriptions of that type ([[ty]]).

The type descriptions contain things such as the target language type
of the ASDL value as well as functions to generate expressions to
construct, deconstruct, and optionally pickle the values. The
functions to pickle the values are carried in a [[ty_info]]
record. This [[ty_info]] record is basically a type dictionary that
could be easily extended to include things like equality testing.
**)

(* TODO abstract ty_info to support more operations *)
signature TYPE_DECL =
  sig
(**:[[signature TYPE_DECL]] opaque types: 
The following types are completely opaque to clients.      
\begin{description}
\item [{[[id]]}] Variable identifiers of the target language.
\item [{[[ty_id]]}] Type identifiers of the target language.
\item [{[[ty_exp]]}] Type expression that represent target language
 representation of an ASDL value. 
\item [{[[tag]]}] Abstract value used to discriminate among the
 [[Sum]] types. 
\item [{[[exp]]}] Expressions of the target language.
\item [{[[env]]}] Abstract environment from [[ty_id]]'s to [[ty]]'s
\end{description}
**)
    structure TypeId : SOURCE_ID
    structure VarId : SOURCE_ID

    type id = VarId.id
    type ty_id = TypeId.id
    type ty_exp
    type tag
    type exp 
    type env
    type ty_info
(**)
(**:[[signature TYPE_DECL]] [[ty]] datatype:**)
    datatype ty =
(**:[[signature TYPE_DECL]] [[ty]] datatype:
The [[Prim]] constructor describes an atomic type.
**)
      Prim of {ty : ty_exp,
	     name : string,
	     info : ty_info}
(**:[[signature TYPE_DECL]] [[ty]] datatype:
The [[Prod]] constructor describes record types made up of other types
described by the [[fields]] field. It also has an info field that may
contain user specified functions to read and write this value. Most
often the [[rd]] and [[wr]] info fields are left empty and readers and
writers are generated automatically.
**)
    | Prod of {ty : ty_exp,
	     info : ty_info,
	   fields : field list,
	    cnstr : exp list -> exp,
	    match : (match list -> exp) -> exp -> exp}
(**:[[signature TYPE_DECL]] [[ty]] datatype:
The [[Sum]] constructor describes a tagged union of values described by
the [[cnstrs]] field. It also has an info field that may contain user
specified functions to read and write this value. Most often the [[rd]]
and [[wr]] info fields are left empty and readers and writers are
generated automatically. The [[num_attrbs]] field describes how many
of the first few fields found in a [[con]] description are actually
attributes in the ASDL spec.
**)
    | Sum  of {ty : ty_exp,
	     info : ty_info,
       num_attrbs : int,
	   cnstrs : con list,
	    match : (choice -> exp) -> exp -> exp}
(**:[[signature TYPE_DECL]] [[ty]] datatype:
The [[Alias]] constructor describes a type alias. Alias are made
explicit so that code generated for one type can be reused for its alias.
**)
    | Alias of  ty_id
(**:[[signature TYPE_DECL]] [[ty]] datatype:
The [[App]] constructors describes the {\em application} of a type
constructor, [[ty_con]], to a particular type identifier. The type
constructor is actually an arbitrary function described by the
[[ty_con]] type which maps a [[ty_decl]] into a target language type
as well as providing specialized readers and writers.  The sequence,
options, and shared ASDL type qualifiers are implemented by defining
an appropriate [[ty_con]].

Notice that [[ty_con]] function takes a [[ty_decl]] which includes 
a [[ty_id]] as well as a [[ty]] value. The [[ty_id]] is provided
since some target languages may require the introduction of new type
or variable identifiers derived from the base type bound to the
[[ty_id]]. 
**)
    | App   of (ty_con * ty_id)
    withtype field   = {label : id option, label' : id, tid : ty_id}
         and match   = (field * exp)
         and choice  = (tag * match list)
         and con = {tag : tag,
		  fields: field list,
                  cnstr : exp list -> exp}
         and ty_decl = (ty_id * ty)
         and ty_con =  ty_decl -> (ty_exp * ty_info)
(**)
(**:[[signature TYPE_DECL]] miscellaneous functions and values:
The [[noInfo]] value is just [[{rd=NONE,wr=NONE}]]. The remaining
functions are for manipulating environments.
**)


    val noInfo  : ty_info

    val addRdWr : string -> {rd:exp option,
			     wr:(exp -> exp) option} -> ty_info -> ty_info
    val getRd   : string -> ty_info -> exp option
    val getWr   : string -> ty_info -> (exp -> exp) option

    val addRd   : (string * exp) -> ty_info -> ty_info
    val addWr   : (string * (exp -> exp)) -> ty_info -> ty_info
    val merge   : (ty_info * ty_info) -> ty_info

    val mk_env  : ty_decl list -> env
    val add_env : (ty_decl * env) -> env
    val lookup  : (env * ty_id) -> ty option
(**)
  end
