(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The [[signature SEMANT]] provides a procedural interface to ASDL
semantic values. The semantic entities in ASDL are module
environments, modules, types, constructors and fields.  After parsing
a set of ASDL module declarations they are processed by
the implementation of [[signature SEMANT]]. The implementation checks
that the declarations are valid, verifying that there are no duplicate
definitions, no recursive module dependencies, or recursively defined
product types. 
**)
signature SEMANT =
    sig
(**:[[signature SEMANT]] opaque types:
Each of the opaque types represent handles to information about specific
semantic entities.
\begin{description}
\item[{[[type menv_info]]}] Describes information about a module
 environment which is a complete and closed set of ASDL modules. ASDL
 module defintions cannot be recursive.
\item[{[[type module_info]]}] Describes information about modules
which are a set of possibly recursive definitons.
\item[{[[type type_info]]}] Describes information about both ASDL product
and sum types.  
\item[{[[type con_info]]}] Describes a constructor for a give type.
\item[{[[type field_info]]}] Describes the fields of a product or sum type.
\end{description}
**)
        type menv_info
	type module_info
	type type_info
	type con_info
	type field_info
(**)
(**:[[signature SEMANT]] [[datatype kind]]:
Since ASDL fields can be modified by various type constructors in the ASDL
source declarations we reuse the [[type_qualifier]] datatype from the AST
in the description of fields.
**)	  
	datatype kind = datatype Asdl.tycon
(**)
(**:[[signature SEMANT]] [[structure Type]]:
The [[Type]] substructure organizes several functions that provide
information about ASDL types. In ASDL source there is a
syntactic distinction between product and sum types.
Here we remove the distinction and simply treat product types as types
without any named constructors.
**)
	structure Type :
	  sig
(**:[[signature SEMANT]] [[structure Type]]:
The [[structure P]] contains query functions for various properties that
can be changed by a view. 
**)
	    structure P  : TYP_PROPS
	    structure Id : SOURCE_ID

	    val props    : type_info -> P.props 
(**:[[signature SEMANT]] [[structure Type]]:
Clients of [[SEMANT]] should ignore [[tag]] it will go away in the future.
**)
	    val tag      : type_info -> int
(**:[[signature SEMANT]] [[structure Type]]:
The functions [[src_name]] and [[name]] return the name of the type
in the {\em output source} and the name used in the ASDL definiton
respectively. Both names are fully qualified and include the name of
the ASDL module in which they were defined. The names are probably a
bit confusing.
**)
	    val src_name : type_info -> Id.id
	    val name     : type_info -> Id.id
(**:[[signature SEMANT]] [[structure Type]]:
[[cons]] returns a list of constructors for a given type
if any.  Product types have no constructors. The [[fields]] function
for a sum type returns the fields found in the [[attributes]]
declaration of the sum type. For product types the [[fields]]
function returns the fields use in the product type declarations.
This allows one to think of product types as a sum type without
any constructors and only attribute fields. 
**)
	    val cons     : type_info -> con_info list
	    val fields   : type_info -> field_info list
(**:[[signature SEMANT]] [[structure Type]]:
The function [[is_prim]] is a predicate to distinguish between primitive
and user defined ASDL types.
**)
	    val is_prim  : type_info -> bool
(**:[[signature SEMANT]] [[structure Type]]:
The function [[is_enum]] is a predicate that returns true iff a sum type
is just a finite enumeration of constants. 
**)
	    val is_enum  : type_info -> bool
(**:[[signature SEMANT]] [[structure Type]]:
[[uses]] simply returns the qualified identifier of types used on the
right hand side of a type declaration. Its internally by the validation
function to construct a dependency graph for the types. It is exported
externally simply for convenience.
**)
	    val uses     : type_info -> Id.id list
	  end
(**)
(**:[[signature SEMANT]] [[structure Con]]:
The [[Con]] substructure organizes several functions that provide information
about type constructors.
**)
	structure Con :
	  sig
(**:[[signature SEMANT]] [[structure Con]]:
The [[structure P]] contains query functions for various properties that
can be changed by a view. 
**)
	    structure P : CON_PROPS
	    structure Id : SOURCE_ID

	    val props          : con_info -> P.props
(**:[[signature SEMANT]] [[structure Con]]:
The [[tag]] function returns an integer that represents the integer tag
used for pickling. 
**)
	    val tag            : con_info -> int
(**:[[signature SEMANT]] [[structure Con]]:
The functions [[src_name]] and [[name]] return the name of the type
in the {\em output source} and the name used in the ASDL definiton
respectively. Both names are fully qualified and include the name of
the ASDL module in which they were defined. The names are probably a
bit confusing.
**)
	    val src_name       : con_info -> Id.id
	    val name           : con_info -> Id.id
(**:[[signature SEMANT]] [[structure Con]]:
The [[fields]] functions return all {\em non-attribute} fields of a
constructor. Non-attribute fields are the fields declared by the
constructor not including any fields declared by the [[attribute]]
keyword of the type.
**)
	    val fields         : con_info -> field_info list
	  end
(**)
(**:[[signature SEMANT]] [[structure Field]]:
The [[Field]] substructure organizes several functions on fields.
Fields have no view modifiable properties. 
**) 
	structure Field :
	  sig
	    structure P  : FIELD_PROPS 
	    structure Id : SOURCE_ID
(**:[[signature SEMANT]] [[structure Field]]:
The function [[kind]] returns any qualifiers (Sequence, Option, or
Shared) for a particular field or [[NONE]] if the field is not
qualified. The [[name]] function returns the name of the optional
label for the field or [[NONE]] if there was no optional label.
The [[src_name]] function returns a uniqe mangle named for fields
without explicit labels or the label used in the specification.
**)	    val props        : field_info -> P.props
	    val kind         : field_info -> kind option
	    val name         : field_info -> Id.id option
	    val src_name     : field_info -> Id.id
	  end
(**)
(**:[[signature SEMANT]] [[structure Module]]:**)
	structure Module :
	  sig
(**:[[signature SEMANT]] [[structure Module]]:
The [[structure P]] contains query functions for various properties that
can be changed by a view. 
**)
	    structure P    : MOD_PROPS 
	    structure Id   : SOURCE_ID

	    val props      : module_info -> P.props
(**:[[signature SEMANT]] [[structure Module]]:
The [[name]] function returns the name of the module as defined in
the ASDL declaration. The [[src_name]] returns the name of the module
as it should occur in the output source. The [[file]] function is a
string that is the native path of the input module from where the
module definition was parsed. 
**)
	    val name       : module_info -> Id.id
	    val src_name   : module_info -> Id.id
	    val file       : module_info -> string
(**:[[signature SEMANT]] [[structure Module]]:
The [[imports]] function returns a list of modules imported by this
module. The [[types]] function returns a list of type identifiers
that are defined by this module. The [[prim_module]] returns true if this
module is a primitive module.
**)
	    val imports    : module_info -> module_info list 
	    val types      : module_info -> Type.Id.id list
	    val prim_module: module_info -> bool
(**:[[signature SEMANT]] [[structure Module]]:
These functions query a module for information about types and constructors 
defined by this module. The [[type_info']] and [[con_info']] behave
the same as [[type_info]] and [[con_info]] but return an option
rather than raising and exception when the type or constructor cannot be found
in the module.
**)	      
	    val type_info  : module_info -> Type.Id.id -> type_info
	    val type_info' : module_info -> Type.Id.id -> type_info option 
	    val con_info   : module_info -> Con.Id.id -> con_info
	    val con_info'  : module_info -> Con.Id.id -> con_info option 
(**:[[signature SEMANT]] [[structure Module]]:
The functions [[con_type]] and [[field_type]] return information
about the type assoicated with a particular constructor or field. For
a constructor the type associated with it is the type it
constructs. For a field the type associated with it is the base type
of the field. 
**)
	    val con_type   : module_info -> con_info -> type_info
	    val field_type : module_info -> field_info -> type_info
(**:[[signature SEMANT]] [[structure Module]]:
Predicate test whether a given type is defined in this module.
**)
	    val is_defined : module_info -> type_info -> bool
	  end
(**)
(**:[[signature SEMANT]] [[structure MEnv]]:
Module Environments don't have a concrete syntactic representation. A
module environment is simply a closed set of module definition. All
types with the exception of the primitive types are defined by some
module in the module environment. 
**)
	structure MEnv :
	  sig
(**:[[signature SEMANT]] [[structure MEnv]]:
The [[structure P]] contains query functions for various properties that
can be changed by a view. 
**)
	    structure P    : MOD_ENV_PROPS 
	    val props      : menv_info -> P.props
(**:[[signature SEMANT]] [[structure MEnv]]:
The [[prim_types]] function returns a list of all the primitive
types of the environment. The [[modules]] function returns a list of
all modules defined in this environment. The [[validate]] functions
takes an environment and checks for circular module or illegal
circular type dependencies. It returns a possibly empty list of error messages.
**)
	    val prim_types : menv_info -> type_info list
	    val modules    : menv_info -> module_info list
	    val validate   : menv_info -> string list
(**:[[signature SEMANT]] [[structure MEnv]]:
The [[qualified]] functions returns a list of all type identifiers
and the  qualifeirs of that type for all types used in the module
envrionment. This is useful for monomorphic languages where special
code needs to be generated for each use of a type qualifier on a given type.
**)
	    val qualified  : menv_info ->
	                     module_info -> (Type.Id.id * kind list) list
(**:[[signature SEMANT]] [[structure MEnv]]:
The [[declare]] function builds a module environment from a list of
AST declarations the name of an "active" view and a list of
initializers for properties associated with the module environment.
It does not validate the constructed environment. Clients need to
call [[validate]] to ensure the resulting environment is consistent.
**)
	    val declare    : {view:string,inits:P.init list} ->
	                     {file:string,decl:Asdl.decl} list ->  menv_info
	  end
(**)
    end
