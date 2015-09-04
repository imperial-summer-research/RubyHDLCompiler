(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These structures and functions are used to maintain the state   ***)
(***    of compilation (e.g. symbol tables, local environments).        ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Mappings:
  sig
        type (''a,'b) mapping
        type (''a,'b) table

        val nullMapping   : (''a ,'b) mapping
        val extendMapping : ((''a*'b) * (''a,'b) mapping) -> (''a,'b) mapping
        val pairs2mapping : (''a*'b) list -> (''a,'b) mapping
        val newTable      : int -> (string,'1a) table
        val store         : (string,'a) table -> (string*'a) -> unit
		(* tjt, Jan 2014: SMLofNJ now seems to need modules to be used explicitly *)
		(*      maybe => Maybe.maybe *)
        val lookup        : (string,'a) table -> string -> 'a Maybe.maybe
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    We begin with mappings:                                         ***)
(***                                                                    ***)
(**************************************************************************)

type (''a,'b) mapping = ''a -> 'b
exception Mapping

fun nullMapping a = raise Mapping

fun extendMapping ((x,v), mapping) a = if    a=x
                                    then  v
                                    else  mapping a

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This function converts any list of pairs to a mapping:          ***)
(***                                                                    ***)
(**************************************************************************)

(* tjt97 - was fold, swap second 2 args *)
fun pairs2mapping xvs = (foldr extendMapping nullMapping xvs)

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    We shall implement larger states using hash tables.  These in   ***)
(***    turn will use the above mappings to resolve clashes:            ***)
(***                                                                    ***)
(**************************************************************************)

type (''a,'b) table      = ((''a,'b) mapping) array

fun newTable size = Array.array (size,nullMapping)

fun store tab (x,v) = let  val index = hash (Array.length tab) x
                           val oldMapping = Array.sub (tab, index)
                           val newMapping = extendMapping ((x,v), oldMapping)
                      in   Array.update (tab, index, newMapping)
                      end

and lookup tab x = let  val index = hash (Array.length tab) x
                   in   Maybe.success ((Array.sub (tab, index)) x)
                   end
                   handle Mapping => Maybe.failure

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    All of the tables used in this compiler are indexed by strings; ***)
(***    thus we only require a hash function for strings:               ***)
(***                                                                    ***)
(***    hash :: string -> int -> int                                    ***)
(***                                                                    ***)
(**************************************************************************)


and hash max str = let  fun hash' [] = 1
                          | hash' (c::cs) = (ord c * hash' cs) mod max
                   in   hash' (explode str)
                   end

(**************************************************************************)
(**************************************************************************)

end (* of structure Mappings *);
(* open Mappings *)
