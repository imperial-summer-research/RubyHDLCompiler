(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These structures represent the final compiled form of ruby      ***)
(***    programs.                                                       ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Circuittype =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    First, let us enumerate the basic devices:                      ***)
(***                                                                    ***)
(**************************************************************************)

datatype device = NOTHING       (* a placeholder; not a real device *)
                | D             (* delay; i.e. a D-type flip-flop *)
				          (* tjt, Jan 2014*)
                | DI   of Rubytype.const (* initialised delay *)
                | MUX  of int   (* n-way multiplexor, integer control *)
                | SDPR of int   (* serial-parallel converter *)
                | PDSR of int   (* parallel-serial converter *)
                | NOT
                | AND
                | OR
                | XOR
                | NAND
                | NOR
                | XNOR
                | LT    (* less than *)
                | GT    (* greater than *)
                | LE    (* less than or equal to *)
                | GE    (* greater than or equal to *)
                | EQ    (* equal to *)
                | IF    (* 2-way multiplexer, boolean control *)
                | BTOI  (* boolean to integer *)
                | ITOB  (* integer to boolean *)
                | ADD
                | SUB
                | MULT
                | DIV
                | MOD
                | EXP
                | LOG
                | MAX
                | MIN
                | GCD
                | FAC
                | DEC
                | INC
                | AD    (* anti-delay *)
                | ABS
                | INT2REAL
                | REAL2INT
                | BIT2UINT  of int
                | BIT2SINT  of int
                | UINT2BIT  of int
                | SINT2BIT  of int
                | BIT2UREAL of int * int
                | BIT2SREAL of int * int
                | UREAL2BIT of int * int
                | SREAL2BIT of int * int
                | DEVICE    of string   (* for special output formats only *)

(**************************************************************************)
(***                                                                    ***)
(***    An expr represents values (constant, variable, monomorphic,     ***)
(***    polymorphic ...) on wires.  Because wires can have more than    ***)
(***    one end, we represent them as references which may be shared.   ***)
(***                                                                    ***)
(***    A single variable wire has fields (direction, id, ends); where  ***)
(***    "direction" indicates whether the wire is for input or output   ***)
(***    (unless polymorphic, in which case it is directionless),        ***)
(***    "id" is a unique integer to identify the wire, and              ***)
(***    "ends" is the number of unconnected ends the wire has.          ***)
(***                                                                    ***)
(**************************************************************************)

(* tjt, Jan 2014 *)
and exprtype    = CON      of Rubytype.const
                | WIRE     of (dir * int * int)
                | LIST     of (expr list)
                | EXPR     of expr            (* created during unification *)

and dir         = IN | OUT | POLY

withtype expr   = exprtype ref

(**************************************************************************)
(***                                                                    ***)
(***    A gate is a device connected to specific input and output wires:***)
(***                                                                    ***)
(**************************************************************************)

type gate       = (device * expr * expr)

(**************************************************************************)
(***                                                                    ***)
(***    We can now define a relation as a set of gates:                 ***)
(***                                                                    ***)
(**************************************************************************)

type relation   = gate list

(**************************************************************************)
(***                                                                    ***)
(***    A circuit is a triple of (domain, range, relation):             ***)
(***                                                                    ***)
(**************************************************************************)

type circuit    = expr * expr * relation

(**************************************************************************)
(***                                                                    ***)
(***    A pcircuit ("printable circuit") is a circuit in which the      ***)
(***    gates have been arranged in a sequence of groups, where the     ***)
(***    inputs of each group depend upon the outputs of previous        ***)
(***    groups:                                                         ***)
(***                                                                    ***)
(**************************************************************************)

type pcircuit   = (expr * expr * relation list)


(**************************************************************************)
(**************************************************************************)

end (* of structure Circuittype *);
(* open Circuittype *)
