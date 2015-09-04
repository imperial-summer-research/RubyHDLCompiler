(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Utilities for ensuring that primitives have the right           ***)
(***    number/types of inputs and outputs:                             ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure PrimTypes :
  sig
        val unary                  : Circuittype.device                 -> Values.value
        val binary                 : Circuittype.device                 -> Values.value
        val ternary                : Circuittype.device                 -> Values.value
        val unary_k                : (Rubytype.const->Circuittype.device)        -> Values.value
        val i_ins_one_out          : (int->Circuittype.device)          -> Values.value
        val one_in_i_outs          : (int->Circuittype.device)          -> Values.value
        val ij_ins_one_out         : ((int*int)->Circuittype.device)    -> Values.value
        val one_in_ij_outs         : ((int*int)->Circuittype.device)    -> Values.value
        val one_and_i_ins_one_out  : (int->Circuittype.device)          -> Values.value

        val num_prim               : (Rubytype.const->Values.value)         -> Values.value
        val num_num_prim           : ((Rubytype.const*Rubytype.const)->Values.value) -> Values.value
        val nat_prim               : (int->Values.value)           -> Values.value
        val nat_nat_prim           : ((int*int)->Values.value)     -> Values.value
        val int_prim               : (int->Values.value)           -> Values.value
        val int_int_prim           : ((int*int)->Values.value)     -> Values.value
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These functions create device primitives.                       ***)
(***    Device primitives have a given number of monomorphic inputs to  ***)
(***    a device and a given number of monomorphic outputs:             ***)
(***                                                                    ***)
(**************************************************************************)

fun unary   dev = Values.CIRCUIT (fn () => let  val ins = Exprs.monoIn 1
                                         val outs = Exprs.monoOut 1
                                    in   (ins, outs, [(dev,ins,outs)])
                                    end
                          )

fun binary  dev = Values.CIRCUIT (fn () => let  val ins = Exprs.list (Exprs.monoInList 1 2)
                                         val outs = Exprs.monoOut 1
                                    in   (ins, outs, [(dev,ins,outs)])
                                    end
                          )

fun ternary dev = Values.CIRCUIT (fn () => let  val ins = Exprs.list (Exprs.monoInList 1 3)
                                         val outs = Exprs.monoOut 1
                                    in   (ins, outs, [(dev,ins,outs)])
                                    end
                          )

(**************************************************************************)
(***                                                                    ***)
(***    This function creates a unary device primitive, whose device is ***)
(***    parameterised by a constant:                                    ***)
(***                                                                    ***)
(**************************************************************************)

fun unary_k dev = Values.FUNCTION (fn k => unary (dev (Values.value2const k)))

(**************************************************************************)
(***                                                                    ***)
(***    This function creates an i-ary device primitive:                ***)
(***                                                                    ***)
(**************************************************************************)

fun i_ins_one_out dev
  = Values.FUNCTION (fn i => let val i' = Values.value2nat i
                      in  Values.CIRCUIT (fn ()=>let  val ins = Exprs.list (Exprs.monoInList 1 i')
                                               val outs = Exprs.monoOut 1
                                          in   (ins, outs, [(dev i',ins,outs)])
                                          end
                                  )
                      end
             )

(**************************************************************************)
(***                                                                    ***)
(***    This function creates an (i,j)-ary device primitive:            ***)
(***                                                                    ***)
(**************************************************************************)

fun ij_ins_one_out dev
  = Values.FUNCTION (fn i => 
    Values.FUNCTION (fn j => 
                   let val i' = Values.value2nat i
                       val j' = Values.value2nat j
                   in  Values.CIRCUIT (fn ()=>let val ins = Exprs.list [Exprs.list (Exprs.monoInList 1 i'),
                                                           Exprs.list (Exprs.monoInList 1 j')]
                                           val outs = Exprs.monoOut 1
                                       in  (ins, outs, [(dev (i',j'),ins,outs)])
                                       end
                                )
                   end
             ))

(**************************************************************************)
(***                                                                    ***)
(***    This function creates a unary device primitive with i           ***)
(***    Exprs.monomorphic outputs:                                            ***)
(***                                                                    ***)
(**************************************************************************)

fun one_in_i_outs dev
  = Values.FUNCTION (fn i => let val i' = Values.value2nat i
                      in  Values.CIRCUIT (fn ()=>let val ins  = Exprs.monoIn 1
                                              val outs = Exprs.list(Exprs.monoOutList 1 i')
                                          in  (ins, outs, [(dev i',ins,outs)])
                                          end
                                  )
                      end
             )

(**************************************************************************)
(***                                                                    ***)
(***    This function creates a unary device primitive with i and j     ***)
(***    Exprs.monomorphic outputs:                                            ***)
(***                                                                    ***)
(**************************************************************************)

fun one_in_ij_outs dev
  = Values.FUNCTION (fn i => 
    Values.FUNCTION (fn j =>  
                  let val i' = Values.value2nat i
                      val j' = Values.value2nat j
                  in  Values.CIRCUIT (fn ()=>let val ins  = Exprs.monoIn 1
                                          val outs = Exprs.list [Exprs.list(Exprs.monoOutList 1 i'),
                                                           Exprs.list(Exprs.monoOutList 1 j')]
                                      in  (ins, outs, [(dev (i',j'),ins,outs)])
                                      end
                              )
                  end
             ))

(**************************************************************************)
(***                                                                    ***)
(***    This function creates a device primitive with a pair of inputs  ***)
(***    (one Exprs.monomorphic wire, n Exprs.monomorphic wires) and one Exprs.monomorphic ***)
(***    output:                                                         ***)
(***                                                                    ***)
(**************************************************************************)

fun one_and_i_ins_one_out dev
  = Values.FUNCTION (
      fn i=>let val i' = Values.value2nat i
            in  Values.CIRCUIT (fn ()=>let val ins = Exprs.list [Exprs.monoIn 1,
                                                    Exprs.list (Exprs.monoInList 1 i')]
                                    val outs = Exprs.monoOut 1
                                in  (ins, outs, [(dev i',ins,outs)])
                                end
                        )
            end
             )

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These functions convert SML functions to Ruby functions which   ***)
(***    accept Ruby values and convert them into the appropriate SML    ***)
(***    types.                                                          ***)
(***                                                                    ***)
(**************************************************************************)

(**************************************************************************)
(***                                                                    ***)
(***    One or two numeric parameters:                                  ***)
(***                                                                    ***)
(**************************************************************************)

fun num_prim f      = Values.FUNCTION (fn n => f (Values.value2num n))

fun num_num_prim f  = Values.FUNCTION (fn n1 =>
                      Values.FUNCTION (fn n2 => f (Values.value2num n1, Values.value2num n2)))

(**************************************************************************)
(***                                                                    ***)
(***    One or two natural number (>=0) parameters:                     ***)
(***                                                                    ***)
(**************************************************************************)

fun nat_prim f     = Values.FUNCTION (fn n => f (Values.value2nat n))

fun nat_nat_prim f = Values.FUNCTION (fn n1 =>
                     Values.FUNCTION (fn n2 => f (Values.value2nat n1, Values.value2nat n2)))

(**************************************************************************)
(***                                                                    ***)
(***    One or two integer parameters:                                  ***)
(***                                                                    ***)
(**************************************************************************)

fun int_prim f     = Values.FUNCTION (fn n => f (Values.value2int n))

fun int_int_prim f = Values.FUNCTION (fn n1 =>
                     Values.FUNCTION (fn n2 => f (Values.value2int n1, Values.value2int n2)))

(**************************************************************************)
(**************************************************************************)

end (* of structure PrimTypes *);
(* open PrimTypes *)
