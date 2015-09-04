(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Sequential and parallel composition of  circuits:               ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Compose :
  sig
        val par : Circuittype.circuit list -> Circuittype.circuit
        val seq : (Circuittype.circuit * Circuittype.circuit) -> Circuittype.circuit
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Parallel composition is very simple: domains are concatenated,  ***)
(***    ranges are concatenated, and the union of all the sets of gates ***)
(***    is taken:                                                       ***)
(***                                                                    ***)
(**************************************************************************)

fun par ps = let  fun par' []      = ([], [], [])
                    | par' (p::ps) = let  val (dom,  ran,  rel)  = p
                                          val (doms, rans, rels) = par' ps
                                     in   (dom::doms, ran::rans, rel@rels)
                                     end
                  val (ds, rs, relation) = par' ps
             in   
			 (* (list ds, list rs, relation)   (*orig*) *)
             (* (ds, rs, relation) (* type error *)  *)
			 (* tjt, Jan 2014: following passes type checking 
			  * note need to make refs from values by using ref (value)
			 (ref (Circuittype.LIST ds), ref (Circuittype.LIST rs), relation)
			  *)
			  (Exprs.list ds, Exprs.list rs, relation)   (* as above: just needed module name *) 
             end

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Sequential composition is more involved: to compose L and R, we ***)
(***    take the domain of L, the range of R, and the union of the sets ***)
(***    of gates from L and R.  In addition, the range of L and the     ***)
(***    domain of R must be unified.                                    ***)
(***                                                                    ***)
(**************************************************************************)

infix :==

fun seq (p, q) = let  val (doml, ranl, rell) = p
                      val (domr, ranr, relr) = q
                      val dummy = unify (ranl, domr)
                 in   (doml, ranr, rell@relr)
                 end

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    To unify individual wires, we use the :== assignment operator   ***)
(***    (defined below).  This operator performs assignment by creating ***)
(***    a new indirection to preserve sharing, while also checking for  ***)
(***    a variety of error conditions:                                  ***)
(***                                                                    ***)
(**************************************************************************)

and unify (x, y)
 = (case (!x, !y) of
      (* tjt97 - add code for each case of CON
	   probably needed because REAL is no longer an equality type!
	   see http://www.smlnj.org/doc/Conversion/types.html
	   (yes, I know that comparing reals is not always a good idea, but
	   making reals somehow non-comparable is silly even by the standards of
	   functional-programming zealots)
      (Circuittype.CON a,  Circuittype.CON b)           => if    a=b
                                   then  ()
                                   else  Errors.seq_error "unequal constants joined"
								   *)
       (Circuittype.CON (Rubytype.BOOL a),  Circuittype.CON (Rubytype.BOOL b))           => if    a=b
                                   then  ()
                                   else  Errors.seq_error "unequal constants joined"
    |  (Circuittype.CON (Rubytype.INT a),  Circuittype.CON (Rubytype.INT b))           => if    a=b
                                   then  ()
                                   else  Errors.seq_error "unequal constants joined"
    |  (Circuittype.CON (Rubytype.REAL a),  Circuittype.CON (Rubytype.REAL b))           => if    Real.==(a,b)
                                   then  ()
                                   else  Errors.seq_error "unequal constants joined"
    |  (Circuittype.CON (Rubytype.SYM a),  Circuittype.CON (Rubytype.SYM b))           => if    a=b
                                   then  ()
                                   else  Errors.seq_error "unequal constants joined"
    | (Circuittype.CON _,  Circuittype.WIRE (Circuittype.OUT,_,_))  => Errors.seq_error "constant joined to output"
    | (Circuittype.CON _,  Circuittype.WIRE _)          => y :== x
    | (Circuittype.CON _,  Circuittype.LIST _)          => Errors.seq_error "constant joined to tuple"

    | (Circuittype.WIRE (Circuittype.OUT, _,_), Circuittype.CON _)  => Errors.seq_error "output joined to constant"
    | (Circuittype.WIRE _,          Circuittype.CON _)  => x :== y

    | (Circuittype.WIRE (Circuittype.OUT, _,_), Circuittype.WIRE (Circuittype.OUT, _,_))  => Errors.seq_error "output wires joined"
    | (Circuittype.WIRE (Circuittype.OUT, _,_), Circuittype.WIRE _)           => y :== x
    | (Circuittype.WIRE (Circuittype.IN,  _,_), Circuittype.WIRE (Circuittype.OUT, _,_))  => x :== y
    | (Circuittype.WIRE (Circuittype.IN,  _,_), Circuittype.WIRE _)           => y :== x
    | (Circuittype.WIRE (Circuittype.POLY,_,e), _)                => x :== y
    | (Circuittype.WIRE _,          Circuittype.LIST _)           => Errors.seq_error
                                            "single wire joined to tuple"

    | (Circuittype.LIST _,  Circuittype.CON _)            => Errors.seq_error "tuple joined to constant"
    | (Circuittype.LIST _,  Circuittype.WIRE (Circuittype.POLY,_,e))  => y :== x
    | (Circuittype.LIST _,  Circuittype.WIRE _)           => Errors.seq_error "tuple joined to single wire"
    | (Circuittype.LIST xs, Circuittype.LIST ys)          => if    length xs = length ys
                                     then  app unify (ListPair.zip (xs, ys))
                                     else  Errors.seq_error
                                           "wires of unequal widths joined"

    | (Circuittype.EXPR e, _)       => unify (e, y)
    | (_,      Circuittype.EXPR e)  => unify (x, e)
   )

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    The :== assignment operator                                     ***)
(***      1) checkes for infinite expansion in the substitution being   ***)
(***         performed                                                  ***)
(***      2) decrements by 2 the "end counts" of the wires being        ***)
(***         joined -- these values record the number of loose ends on  ***)
(***         each wire                                                  ***)
(***      3) perform the assignment through an indirection (EXPR), to   ***)
(***         preserve sharing of information amongst the ends of a wire ***)
(***                                                                    ***)
(**************************************************************************)

and x :== y = let  fun inexpr y = if   x=y
                                  then true
                                  else inexprtype (!y)
                   and inexprtype (Circuittype.LIST es)     = List.exists inexpr es
                     | inexprtype (Circuittype.EXPR e)      = inexpr e
                     | inexprtype _             = false
              in   if    inexpr y
                   then  Errors.seq_error "incompatable wires joined"
                   else  let  val xends = Exprs.endCount x
                              val dummy = addToEndCount (xends-2) y
                         in   x := Circuittype.EXPR y
                         end
              end

and addToEndCount n e
        = (case !e of
             Circuittype.CON _                => ()
           | Circuittype.WIRE (dir,name,ends) => if    dir=Circuittype.IN andalso ends+n=0
                                     then  Errors.seq_error "undriven input wire"
                                     else  e := Circuittype.WIRE (dir, name, ends+n)
           | Circuittype.LIST es              => app (addToEndCount n) es
           | Circuittype.EXPR x               => addToEndCount n x
          )

(**************************************************************************)
(**************************************************************************)

end (* of struct Compose *);
(* open Compose *)
