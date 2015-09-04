(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    A few elements of global state:                                 ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure State:
  sig
        val SourceFiles  : string list ref
        val IncludeFiles : string list ref
        val FunStack     : string list ref
        val CurrentPrim  : string ref
        val Testing      : bool ref
        val DefTabSize   : int

        val pushFun      : string -> unit
        val popFun       : string -> unit
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***    Files which have already been loaded; used for avoiding INCLUDE ***)
(***    cycles:                                                         ***)

val SourceFiles  = ref [] : string list ref

(**************************************************************************)
(***    Files which have yet to be loaded:                              ***)

val IncludeFiles = ref [] : string list ref

(**************************************************************************)
(***    Call-stack of user-defined functions; used only for             ***)
(***    error reporting:                                                ***)

val FunStack   = ref [] : string list ref
fun pushFun f  = FunStack := f :: !FunStack
fun popFun _   = FunStack := tl (!FunStack)

(**************************************************************************)
(***    Built-in function currently being evaluated; used only for      ***)
(***    error reporting:                                                ***)

val CurrentPrim  = ref ""

(**************************************************************************)
(***    True iff debugging:                                             ***)

val Testing      = ref false

(**************************************************************************)
(***    Size of symbol table; recall however that entries in the table  ***)
(***    are chained, thus we can store more than this number of symbols ***)
(***    in the table:                                                   ***)

val DefTabSize   = 1024

(**************************************************************************)
(**************************************************************************)

end (* of structure State *);
(* open State *)
