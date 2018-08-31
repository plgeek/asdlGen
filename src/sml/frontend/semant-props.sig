(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(* TODO write a tool to take a property description and compile it
 * to code
 *)
(**::
Various semantic entities contain user definable properties, which
can be set with user views. To make the set of properties easily
extensible we use what amounts to an extensible record type. In order
to achieve type safety each extensible record is hidden behind several
abstract signatures.  
**)
(**:[[signature PROPS]]:
Description of a [[prop]] type. Properties are built by passing in a
set of abstract initializers. Some initializers have a parseable
representation. The function [[parse]] takes key value pairs and
builds a set of initializers based on the key value pairs.
Other signatures extend this basic pattern with functions to project
out of the [[props]] type into values.
**)
signature PROPS =
  sig
    type props
    type init
    val new: init list -> props
    val parse: (string * string) list -> init list
  end
(**)
(**:[[signature COMMON_PROPS]]:
Set of common properties. The [[source_name]] property is an optional
string that allows a user view to override the default name of a
entity in the source that is generated. The [[doc_string]] property
is a human readable string descriping some entity.
**)
signature COMMON_PROPS =
  sig	include PROPS
    val source_name         :  props -> Properties.path option
    val doc_string          :  props -> string option
  end
(**)
(**:[[signature FIELD_PROPS]]:
Properties for fields.
**)
signature FIELD_PROPS =
  sig	include COMMON_PROPS
    val init_source_name : Properties.path option -> init
  end
(**)
(**:[[signature TYP_PROPS]]: 
Some properties for types.
**)
signature TYP_PROPS =
  sig include COMMON_PROPS 
(**:[[signature TYP_PROPS]]: 
These properties allow users to add extra fields to the generated
code. The [[user_attribute]] property is the name of a type for an
additional user defined field added to records. The [[user_init]]
property defines an initialization function for the user defined
types. In object oriented languges the [[base_class]] property
determines the base class of all classes generated. These properties
are output language specfic.
**)
    val user_attribute      : props -> Properties.path option
    val user_init           : props -> Properties.path option
    val base_class          : props -> Properties.path option
(**:[[signature TYP_PROPS]]: 
All languages support properties that allow users to define
specialized readers and writer functions for a particular type.
**)      
    val reader              : props -> Properties.path option
    val writer              : props -> Properties.path option
(**:[[signature TYP_PROPS]]: 
The [[natural_type]] and [[natural_type_con]] allow user views to
overide the representation of a type in the generated source. These
should be used in conjunction with the [[wrapper]] and [[unwrapper]]
to define coercion functions that convert to and from the standard generic
representation and a user defined specialized representation. The
[[wrapper]] property the name of a coercion function that translates
from the specialized type to the default generic representation.
The [[unwrapper]] property specifies a coercion function to do the reverse.
**)      
    val natural_type        : props -> Properties.path option
    val natural_type_con    : props -> Properties.path option
    val wrapper             : props -> Properties.path option
    val unwrapper           : props -> Properties.path option
  end
(**)
(**:[[signature CON_PROPS]]:
For languages that have explicit tag values in the representation
(C, C++, Java) choose a particular tag value. These tag values are
the values used in the in core representation not the actual pickle.
**)
signature CON_PROPS =
  sig include COMMON_PROPS
    val enum_value          :  props -> int option
  end
(**)
(**:[[signature MOD_PROPS]]:
These are module level properties. Moste of these should go
away in the future.
**)
signature MOD_PROPS =
  sig include COMMON_PROPS
    val file                   : props -> string
    val mk_file                : string -> init
    val custom_allocator       : props -> string option
    val interface_prologue     : props -> string
    val interface_epilogue     : props -> string
    val implementation_prologue: props -> string
    val implementation_epilogue: props -> string
    val suppress               : props -> bool
    val is_library             : props -> bool
  end 
(**)
(**:[[signature MOD_ENV_PROPS]]:
These are module environment properties. Most of them are set through
command line options rather than the normal view mechanism. 
**)
signature MOD_ENV_PROPS =
  sig include COMMON_PROPS
    val mono_types            : props -> bool
    val init_mono_types       : bool -> init
    val aux_mod_suffix        : props -> string option
    val init_aux_mod_suffix   : string option -> init
    val pickler_kind          : props -> string 
    val init_pickler_kind     : string -> init
    val explicit_sharing      : props -> bool
    val init_explicit_sharing : bool -> init
  end
(**)