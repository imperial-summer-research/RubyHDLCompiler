(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Utilities for manipulating exprs:                               ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Exprs:
  sig
        val MonoWireCount : int ref
        val PolyWireCount : int ref

(* tjt, Jan 2014: const => Rubytype.const, expr => Circuittype.expr *)
        val con         : Rubytype.const      -> Circuittype.expr
        val monoIn      : int        -> Circuittype.expr
        val monoOut     : int        -> Circuittype.expr
        val poly        : int        -> Circuittype.expr
        val monoInList  : int -> int -> Circuittype.expr list
        val monoOutList : int -> int -> Circuittype.expr list
        val polyList    : int -> int -> Circuittype.expr list
        val list        : Circuittype.expr list  -> Circuittype.expr

        val exprInputs  : Circuittype.expr -> int list
        val exprOutputs : Circuittype.expr -> int list
        val exprWires   : Circuittype.expr -> Circuittype.expr list
        val flattenExpr : Circuittype.expr -> Circuittype.exprtype list
        val endCount    : Circuittype.expr -> int

  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Here are a few funcions for creating new exprs:                 ***)
(***                                                                    ***)
(**************************************************************************)


val MonoWireCount = ref 0;
val PolyWireCount = ref 0;

fun nextMonoName () = (MonoWireCount := 1 + !MonoWireCount;
                       !MonoWireCount)
fun nextPolyName () = (PolyWireCount := 1 + !PolyWireCount;
                       !PolyWireCount)

(* tjt, Jan 2014 *)
fun con k      = ref (Circuittype.CON k)

(* tjt, Jan 2014 *)
fun monoIn  ends = ref (Circuittype.WIRE (Circuittype.IN,   nextMonoName(), ends))
fun monoOut ends = ref (Circuittype.WIRE (Circuittype.OUT,  nextMonoName(), ends))
fun poly    ends = ref (Circuittype.WIRE (Circuittype.POLY, nextPolyName(), ends))

(* tjt, Jan 2014 *)
fun monoInList  ends n = Utilities.for (1, n) (fn x => monoIn  ends)
fun monoOutList ends n = Utilities.for (1, n) (fn x => monoOut ends)
fun polyList    ends n = Utilities.for (1, n) (fn x => poly    ends)

fun list es    = ref (Circuittype.LIST es)

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    and utilities to find the input and output wires of an expr:    ***)
(***                                                                    ***)
(**************************************************************************)

(* tjt, Jan 2014 *)
fun exprInputs e
        = (case !e of
(* tjt, Jan 2014 *)
             Circuittype.CON _             => []
           | Circuittype.WIRE (Circuittype.IN,  i,_)   => [i]
           | Circuittype.WIRE (Circuittype.OUT, _,_)   => []
           | Circuittype.WIRE (Circuittype.POLY,i,_)   => [i]
           | Circuittype.LIST es           => Utilities.flatmap exprInputs es
           | Circuittype.EXPR x            => exprInputs x
          )

fun exprOutputs e
        = (case !e of
(* tjt, Jan 2014 *)
             Circuittype.CON _             => []
           | Circuittype.WIRE (Circuittype.IN,  _,_)   => []
           | Circuittype.WIRE (Circuittype.OUT, i,_)   => [i]
           | Circuittype.WIRE (Circuittype.POLY,_,_)   => []
           | Circuittype.LIST es           => Utilities.flatmap exprOutputs es
           | Circuittype.EXPR x            => exprOutputs x
          )

fun exprWires e
        = (case !e of
(* tjt, Jan 2014 *)
             Circuittype.CON _             => []
           | Circuittype.WIRE _            => [e]
           | Circuittype.LIST es           => Utilities.flatmap exprWires es
           | Circuittype.EXPR x            => exprWires x
          )

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This function gives a list of all constant and variable wires   ***)
(***    in an expr:                                                     ***)
(***                                                                    ***)
(**************************************************************************)

fun flattenExpr e = (case !e of
(* tjt, Jan 2014 *)
                       Circuittype.LIST es  => Utilities.flatmap flattenExpr es
                     | Circuittype.EXPR x   => flattenExpr x
                     | x        => [x]
                    )

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This function gives the number of loose ends of a wire:         ***)
(***                                                                    ***)
(**************************************************************************)

fun endCount e = (case !e of
                    Circuittype.CON _        => 0
                  | Circuittype.WIRE (_,_,e) => e
(* tjt, Jan 2014 *)
                  | Circuittype.LIST es      => Utilities.sum (map endCount es)
                  | Circuittype.EXPR x       => endCount x
                 )

(**************************************************************************)
(**************************************************************************)

end (* of structure Exprs *);
(* open Exprs *)
