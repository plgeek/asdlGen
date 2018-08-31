(**::
Building software can be quite complex. Traditional build
systems like [[make]] offer the right set of primitives, but a very
weak set of functions to compose these primitives into modular and
reliable specifications for building software. Building software
written in several different langauges on more than one platform
platform using plain [[make]] files can quickly be come a combinatorial
nightmare.  Systems such as [[automake]] which
generate [[make]] files automatically attempt to address this issue but
aren't sufficiently general enough to be easy to extend.
 
In order to make build software easier, I've coded up the core set of
make primitives in Standard ML. From these core primitives you can use
the full power of Standard ML to compose a non-trival build
specification. This system provides two operations on the core
primitives. One is a simple straightforward interpreter the other a
function that compiles the description into a [[make]] file, suitable
for most standard makes.

This system does not address the issues of configuration that systems
like GNU [[autoconf]] attempt to solve. The best approach is to
parameterize build descriptions in such a way that [[autoconf]] can
easily automatically generate the right platform specific information
needed for the build, and feed that information into your build
description.

The one drawback with this approach is that lots of build code is
tedious and describing things in ML style syntax isn't always the
nicest way to do things. A pre-processor that would take a more
[[make]] like syntax plus escapes into ML code would make things look
a bit cleaner. The quasi-quote feature of SML/NJ is also worth looking
at. The real win is having functions and real abstraction mechanisms
in your build language.

The current set of libraries built on top of the primitives are
structured such that you can build and install software on Win32 and a
standard Unix box from one parametrized specification. Using the
native compilers, linkers, and library archivers on both systems.
**)
(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**:[[signature CORE_BUILD]]:
 The [[CORE_BUILD]] signature describes the basic build primitives,
 found from which more complicated primtives can be built from.
**)
signature CORE_BUILD =
  sig
    type 'a cmd
    type var
    type valid
    type rule

    val mkVAR : {name:string option,doc:string list,init:string cmd} -> var
    val VAR   : var -> string cmd 
    val STR   : string          -> string cmd
    val INT   : int             -> int cmd
(**:[[signature CORE_BUILD]]:
\begin{description}     
\item [{[[WRITE]]}] Output a string for diagnostic purpose.
 Just like \verb|echo s| in a [[make]] file.
\item [{[[CONCAT]]}] Concatenate a list of string into one string.
\item [{[[EXEC]]}] Call a program described by a variable with a list
 of arguments, we force the program name to be a variable to encourage
 modular build specs. Just like \verb|$(var) s1 s2 ... sn|.
\item [{[[EXEC_WITH_INPUT]]}] Just like [[EXEC]] but use a file as the
source of input.  Like \verb|$(var) s1 s2 ... sn < f1| in a [[make]] file.
\item [{[[EXIT]]}] Exit with an error message.
\end{description}
**)
    val WRITE : string cmd -> unit cmd
    val CONCAT: string cmd list -> string cmd
    val EXEC  : (var * string cmd list) -> unit cmd
    val EXEC_WITH_INPUT : (var * string cmd list * string cmd) -> unit cmd
    val EXIT  : string cmd -> 'a cmd 
(**:[[signature CORE_BUILD]]: Standard [[make]] style control flow operators.
**)
    val OR    : 'a cmd list -> unit cmd
    val AND   : 'a cmd list -> unit cmd
    val SEQ   : 'a cmd list -> unit cmd
    val IGN   : 'a cmd -> unit cmd
(**:[[signature CORE_BUILD]]:[[make]] style rules.
**)
    val INVALID  : valid 
    val VALIDATE : {targets:Paths.file_path list,
		    depends:Paths.file_path list} -> valid 
    val RULE  : {valid:valid,update:unit cmd} -> rule
(**:[[signature CORE_BUILD]]:
One novel feature is you can group rules into a single command. This
corresponds to recursively invoking make on some [[make]] file
with rules in it.
**)      
    val BUILD : {name:string option,rules:rule list} -> unit cmd
  end
(**)
(**:[[signature META_BUILD]]:
The [[META_BUILD]] signature is a refinement of the [[CORE_BUILD]] signature 
that provides functions for interpreting a build description or
exporting it to a real [[make]] file.
**)
signature META_BUILD =
  sig
    include CORE_BUILD
    val setVAR : var -> string -> unit 
    val run    : unit cmd -> bool
    val export : TextIO.outstream -> unit cmd -> unit
  end
(**)
