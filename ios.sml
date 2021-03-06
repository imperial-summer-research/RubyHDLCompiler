(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Utilities for dealing with ios:                                 ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Ios:
  sig
        (* tjt, Jan 2014: io => Rubytype.io*)
        val iovars : Rubytype.io -> string list
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This utility makes a list of all of the variable wire names     ***)
(***    which appear in an io structure:                                ***)
(***                                                                    ***)
(**************************************************************************)

(* tjt, Jan 2014: Rubytype, Utilities *)
fun iovars (Rubytype.VAR x)   = [x]
  | iovars (Rubytype.PROD ws) = Utilities.flatmap iovars ws
  | iovars _         = []

(**************************************************************************)
(**************************************************************************)

end (* of structure Ios *);
(* open Ios *)
