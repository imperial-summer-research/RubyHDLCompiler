(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These functions give a printable string representation of a     ***)
(***    pcircuit.                                                       ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Printrbs :
  sig
        val showRBS : Circuittype.pcircuit -> string
  end =
struct

(**************************************************************************)
(**************************************************************************)

fun showExpr e = (case !e of
                    Circuittype.CON k       => Show.showConst k
                  | Circuittype.WIRE (Circuittype.POLY,name,_) => "." ^ Int.toString name (* use ":"? *)
                  | Circuittype.WIRE (_,   name,_) => "." ^ Int.toString name
                  | Circuittype.LIST es            => "<" ^
                                          Utilities.concatWith "," (map showExpr es) ^
                                          ">"
                  | Circuittype.EXPR x             => showExpr x
                 )

fun showGate (device, input, output)
        = "   " ^
          Utilities.ljustify 15 (Show.showDevice device) ^
          Utilities.ljustify 19 (showExpr input) ^
                       showExpr output ^ "\n"

fun showRelation ds =Utilities.concatWith "" (map showGate ds)

fun showRelations [] = ""
  | showRelations rs = let  val sep = "   ----------------------------------------\n"
                       in   "\n   Name           Domain             Range\n" ^
                            sep ^
                           Utilities.concatWith sep (map showRelation rs) ^
                            sep
                       end

fun showWiring (dom, ran) = "\n Wiring -  " ^
                            showExpr dom ^ " ~ " ^ showExpr ran ^ "\n"

fun showInputs (dom, ran)
        
        = let  fun isInput e = (case !e of
                                  Circuittype.WIRE (Circuittype.IN  ,_,_)  => true
                                | Circuittype.WIRE (Circuittype.POLY,_,_)  => true
                                | _                => false
                               )
							   
	    (*
        = let  fun isInput e = (case !e of
                                  WIRE (IN  ,x0,x1)  => true
                                | WIRE (POLY,x2,x3)  => true
                                | WIRE (OUT,x4,x5)   => false
								| LIST (_)           => false
                               )
							   *)
               val wires = Exprs.exprWires dom @ Exprs.exprWires ran
			   (* tjt97: was:
               val inputs = orderedRemduprefs (filter isInput wires)
			   *)
			   (*
               val inputs = tjt97remDupRefs (filter isInput wires)
			   *)
               val inputs = Utilities.filter isInput wires
			   (*tjt: since it seems impossible in current SML to remove duplicates from a
			   list of references, remove the duplicates after references have been
			   printed to strings *)
               val inputStrs = (map showExpr inputs)
			   val inputUniqStrs = Utilities.orderedRemdups inputStrs
               val inStr = if    null inputs
                           then  "none"
                           else 
						      Utilities.concatWith " " inputUniqStrs
						      (* tjt: was
						      Utilities.concatWith " " (map showExpr inputs)
							  *)
          in   "\n Inputs -  " ^ inStr ^ "\n"                
          end

fun showDirections (dom, ran)
        = let  fun showDir e = (case !e of
                                  Circuittype.CON k           => "out"
                                | Circuittype.WIRE (Circuittype.IN,  _,_) => "in"
                                | Circuittype.WIRE (Circuittype.OUT, _,_) => "out"
                                | Circuittype.WIRE (Circuittype.POLY,_,_) => "?"
                                | Circuittype.LIST es         => "<" ^ Utilities.concatWith ","
                                                           (map showDir es) ^
                                                  ">"
                                | Circuittype.EXPR x          => showDir x
                               )
          in   "\n Directions -  " ^ showDir dom ^ " ~ " ^ showDir ran ^ "\n"
          end

fun showRBS (dom, ran, rels)
        = showRelations (Utilities.filter (not o null) rels) ^
          showDirections (dom, ran) ^
          showWiring (dom, ran) ^
          showInputs (dom, ran)

(**************************************************************************)
(**************************************************************************)

end (* of structure Printrbs *);
(* open Printrbs *)
