(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This file contains routines for ordering the gates of a circuit ***)
(***    into a list of lists of gates, representing a sequence of       ***)
(***    parallel blocks.                                                ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Order :
  sig
        val circuit2pcircuit : Circuittype.circuit -> Circuittype.pcircuit
  end =
struct

(**************************************************************************)
(**************************************************************************)
(*
        The input and output wires of a gate:
*)

fun inputs (dev, inputExpr, outputExpr)
        = Utilities.remdups (Exprs.exprInputs inputExpr)

fun outputs (dev, inputExpr, outputExpr)
        = Utilities.remdups (Exprs.exprOutputs outputExpr)

(**************************************************************************)
(***                                                                    ***)
(***    To avoid repeated searches through a list of gates, we create   ***)
(***    an array of gates, indexed by output wire.  In each entry, we   ***)
(***    record the gate, a list of (non-constant) input wires to the    ***)
(***    gate, and the maximum distance from that gate to circuit inputs ***)
(***    and/or D (delay) outputs:                                       ***)
(***                                                                    ***)
(**************************************************************************)

type gateRecord = (Circuittype.gate * int list * int)

val NullRecord = ((Circuittype.NOTHING, Exprs.list [], Exprs.list []), [], 0) : gateRecord;

val GateArray = ref (Array.array (0, NullRecord));

fun installGate (g as (dev, inExpr, outExpr))
    = let  val ins  = Utilities.remdups (Exprs.exprInputs inExpr @ Exprs.exprOutputs inExpr)
           val outs = Utilities.remdups (Exprs.exprOutputs outExpr)
           fun install i = Array.update (!GateArray, i, (g,ins,0))
      in   app install outs
      end;

(**************************************************************************)
(***                                                                    ***)
(***    This function is used only after a circuit loop has been found, ***)
(***    for generating the error report:                                ***)
(***                                                                    ***)
(**************************************************************************)

fun findLoops w0 w
    = if    w=w0
      then  [[]]
      else  (case Array.sub (!GateArray, w) of
               ((Circuittype.NOTHING,_,_),_,_)   => []
             | ((Circuittype.D,      _,_),_,_)   => []
             | ((Circuittype.DI _,   _,_),_,_)   => []
             | ((dev,_,_),inputs,_)  => map (fn path => dev::path)
                                            (Utilities.flatmap (findLoops w0) inputs)
      )

(**************************************************************************)
(***                                                                    ***)
(***    Find (and record) the distance of a particular wire.            ***)
(***                                                                    ***)
(**************************************************************************)

val MaxDistance = ref 0;

fun distance wire
  = (case Array.sub (!GateArray, wire) of
       ((Circuittype.NOTHING,_,_),_,_)   => 0   (* must be an input to the circuit *)

     | ((Circuittype.D,   _,_),_,_)      => 0   (* output not dependent on any input *)
     | ((Circuittype.DI _,_,_),_,_)      => 0

     | (gate,inputs,0) => (Array.update (!GateArray, wire, (gate,inputs,~1));
                           let val dist = 1 + Utilities.listmax (map distance inputs)
                           in  (Array.update(!GateArray,wire,(gate,inputs,dist));
                                if    dist > !MaxDistance
                                then  MaxDistance := dist
                                else  ();
                                dist)
                           end
                          )

     | ((dev,_,_),inputs,~1) => Errors.loop_error (map Show.showDevice
                                 (dev :: hd (Utilities.flatmap (findLoops wire) inputs)))

     | (_, _, dist)          => dist   (* has already been calculated *)
    )

(**************************************************************************)
(***                                                                    ***)
(***    Having found all the distances, we can group gates according    ***)
(***    to those distances:                                             ***)
(***                                                                    ***)
(**************************************************************************)

val ParBlocks = ref (Array.array (0, [])) : Circuittype.gate list array ref;

fun assignGateToParBlock wire
        = (case Array.sub (!GateArray, wire) of
             ((Circuittype.NOTHING,_,_),_,_) => ()
           | (gate,_,dist)  => Array.update (!ParBlocks,
                                             dist,
                                             Array.sub(!ParBlocks,dist)@[gate])
          )

fun collectPars () = Utilities.for (1, !Exprs.MonoWireCount) assignGateToParBlock

fun order gates = (GateArray := Array.array ((!Exprs.MonoWireCount)+1, NullRecord);
                   app installGate gates;
                   Utilities.for (1, !Exprs.MonoWireCount) distance;
                   ParBlocks := Array.array ((!MaxDistance)+1, []);
                   collectPars ();
				   (* tjt97 - do not remove duplicates again
                   map Utilities.remdups (filter (not o null) (array2list (!ParBlocks)))
				   *)
                   (Utilities.filter (not o null) (Utilities.array2list (!ParBlocks)))
                  )

(**************************************************************************)

fun renameWiresFrom n []
        = n
  | renameWiresFrom n (e::es) = let  val Circuittype.WIRE (dir, name, ends) = !e
                                in   (e := Circuittype.WIRE (dir, n, ends);
                                      renameWiresFrom (n+1) es
                                     )
                                end

(**************************************************************************)
(***                                                                    ***)
(***    Order the gates, and give all monomorphic wires new identifiers ***)
(***    so that they will be contiguous from 1:                         ***)
(***                                                                    ***)
(**************************************************************************)

fun circuit2pcircuit (dom, ran, rel)
        = let  val rels = order rel
               fun gateWires (dev,inExpr,outExpr) = Exprs.exprWires inExpr @
                                                    Exprs.exprWires outExpr
               fun relationWires gates = Utilities.flatmap gateWires gates
               val wires = Utilities.orderedRemdups (Utilities.flatmap relationWires rels @
                                           Exprs.exprWires dom @
                                           Exprs.exprWires ran)
               val dummy = Exprs.MonoWireCount := renameWiresFrom 1 wires
          in   (dom, ran, rels)
          end

(**************************************************************************)
(**************************************************************************)

end (* of structure Order *);
(* open Order *)
