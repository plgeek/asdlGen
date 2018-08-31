(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature TRANSLATE =
    sig
	type input
	type output
	val opts : CommandOptions.args_spec
	val translate: CommandOptions.args -> input -> output
    end














