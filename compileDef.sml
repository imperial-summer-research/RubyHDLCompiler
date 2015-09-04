(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Functions for filling the symbol table with value definitions   ***)
(***    given by the ruby source:                                       ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure CompileDef :
  sig
        val storeDef  : Rubytype.defn -> unit
        val globalDef : string -> Values.value
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    First, here is the symbol table.  Its entries are functions --  ***)
(***    this is so that the entries can be refered to before they are   ***)
(***    fully defined.                                                  ***)
(***                                                                    ***)
(**************************************************************************)

val defTab   = Mappings.newTable State.DefTabSize :    (string, unit -> Values.value) Mappings.table;

(**************************************************************************)
(***                                                                    ***)
(***    storeDef records the information in a definition --             ***)
(***    by installing a new entry in the symbol table, or assigning to  ***)
(***    a global DATA variable, or putting a filename on the list for   ***)
(***    inclusion:                                                      ***)
(***                                                                    ***)
(**************************************************************************)


fun storeDef (Rubytype.Fdefn (f,xs,e))                   (* Fdefn in rubytype.sml *)
        = (State.pushFun f;                           (* for error reporting *)

           (if       xs <> Utilities.remdups xs
            then     Errors.def_error "repeated parameter"
            else if  (Maybe.successful (Mappings.lookup defTab f)  orelse Maybe.successful (prim f))
                     andalso not (!State.Testing)
            then     Errors.def_error "already defined"
            else     Mappings.store defTab (f, fn () => def2value f globalDef (xs,e)));

           State.popFun f)                            (* lookup, store in mappings.sml *)

  | storeDef (Rubytype.Ddefn xe)
        = Data.recordData xe

  | storeDef (Rubytype.Include n)
        = State.IncludeFiles := n :: (!State.IncludeFiles)

(**************************************************************************)
(***                                                                    ***)
(***    def2value takes the name of the function being defined,         ***)
(***    an environment mapping names to values, and a ruby definition   ***)
(***    in the form of a list of parameters and a ruby  expression.     ***)
(***    It returns the definition as a value, which will be a circuit   ***)
(***    or constant, or a function which creates its value in the       ***)
(***    environment extended to map its parameter to an argument        ***)
(***    (ie implements currying)                                        ***)
(***                                                                    ***)
(**************************************************************************)

and def2value name env ([],    e)
        = #2 (State.pushFun name,
              CompileExp.exp2value name env e,
              State.popFun name)

  | def2value name env (x::xs, e)  (* name is name of fn and xs is parameter *)
        = Values.FUNCTION (fn v => def2value name (Mappings.extendMapping ((x,v), env))
                                           (xs,e)
                   )

(**************************************************************************)
(***                                                                    ***)
(***    globalDef maps a name to its value, which may be built-in or    ***)
(***    user-defined:                                                   ***)
(***                                                                    ***)
(**************************************************************************)

and globalDef x = let  val p = prim x
                  in   if    Maybe.successful p
                       then  (State.CurrentPrim := x;    (* for error reporting *)
                              Maybe.the p)
                       else  let  val fundef = Mappings.lookup defTab x
                             in   if    Maybe.successful fundef
                                  then  Maybe.the fundef ()
                                  else  Errors.def_error (x ^ " undefined")
                             end
                  end

and prim "D"        = Maybe.success (PrimTypes.unary    Circuittype.D)
  | prim "DI"       = Maybe.success (PrimTypes.unary_k  Circuittype.DI)
  | prim "not"      = Maybe.success (PrimTypes.unary    Circuittype.NOT)
  | prim "inc"      = Maybe.success (PrimTypes.unary    Circuittype.INC)
  | prim "dec"      = Maybe.success (PrimTypes.unary    Circuittype.DEC)
  | prim "AD"       = Maybe.success (PrimTypes.unary    Circuittype.AD)
  | prim "fac"      = Maybe.success (PrimTypes.unary    Circuittype.FAC)
  | prim "bit2int"  = Maybe.success (PrimTypes.unary    Circuittype.BTOI)
  | prim "int2bit"  = Maybe.success (PrimTypes.unary    Circuittype.ITOB)
  | prim "exp"      = Maybe.success (PrimTypes.unary    Circuittype.EXP)
  | prim "log"      = Maybe.success (PrimTypes.unary    Circuittype.LOG)
  | prim "int2real" = Maybe.success (PrimTypes.unary    Circuittype.INT2REAL)
  | prim "real2int" = Maybe.success (PrimTypes.unary    Circuittype.REAL2INT)
  | prim "abs"      = Maybe.success (PrimTypes.unary    Circuittype.ABS)
  | prim "minus"    = Maybe.success (PrimTypes.binary   Circuittype.SUB)
  | prim "add"      = Maybe.success (PrimTypes.binary   Circuittype.ADD)
  | prim "mult"     = Maybe.success (PrimTypes.binary   Circuittype.MULT)
  | prim "div"      = Maybe.success (PrimTypes.binary   Circuittype.DIV)
  | prim "mod"      = Maybe.success (PrimTypes.binary   Circuittype.MOD)
  | prim "eq"       = Maybe.success (PrimTypes.binary   Circuittype.EQ)
  | prim "sml"      = Maybe.success (PrimTypes.binary   Circuittype.LT)
  | prim "lgr"      = Maybe.success (PrimTypes.binary   Circuittype.GT)
  | prim "ltn"      = Maybe.success (PrimTypes.binary   Circuittype.LT)
  | prim "gtn"      = Maybe.success (PrimTypes.binary   Circuittype.GT)
  | prim "leq"      = Maybe.success (PrimTypes.binary   Circuittype.LE)
  | prim "geq"      = Maybe.success (PrimTypes.binary   Circuittype.GE)
  | prim "gcd"      = Maybe.success (PrimTypes.binary   Circuittype.GCD)
  | prim "max"      = Maybe.success (PrimTypes.binary   Circuittype.MAX)
  | prim "min"      = Maybe.success (PrimTypes.binary   Circuittype.MIN)
  | prim "and"      = Maybe.success (PrimTypes.binary   Circuittype.AND)
  | prim "or"       = Maybe.success (PrimTypes.binary   Circuittype.OR)
  | prim "xor"      = Maybe.success (PrimTypes.binary   Circuittype.XOR)
  | prim "nand"     = Maybe.success (PrimTypes.binary   Circuittype.NAND)
  | prim "nor"      = Maybe.success (PrimTypes.binary   Circuittype.NOR)
  | prim "xnor"     = Maybe.success (PrimTypes.binary   Circuittype.XNOR)
  | prim "if"       = Maybe.success (PrimTypes.ternary  Circuittype.IF)
  | prim "sdpr"     = Maybe.success (PrimTypes.one_in_i_outs   Circuittype.SDPR) (* one_in_i_outs in primTypes *)
  | prim "uint2bit" = Maybe.success (PrimTypes.one_in_i_outs   Circuittype.UINT2BIT)
  | prim "sint2bit" = Maybe.success (PrimTypes.one_in_i_outs   Circuittype.SINT2BIT)
  | prim "ureal2bit" = Maybe.success (PrimTypes.one_in_ij_outs Circuittype.UREAL2BIT)
  | prim "sreal2bit" = Maybe.success (PrimTypes.one_in_ij_outs Circuittype.SREAL2BIT)
  | prim "pdsr"      = Maybe.success (PrimTypes.i_ins_one_out  Circuittype.PDSR)
  | prim "bit2uint"  = Maybe.success (PrimTypes.i_ins_one_out  Circuittype.BIT2UINT)
  | prim "bit2sint"  = Maybe.success (PrimTypes.i_ins_one_out  Circuittype.BIT2SINT)
  | prim "bit2ureal" = Maybe.success (PrimTypes.ij_ins_one_out Circuittype.BIT2UREAL)
  | prim "bit2sreal" = Maybe.success (PrimTypes.ij_ins_one_out Circuittype.BIT2SREAL)
  | prim "muxr"      = Maybe.success (PrimTypes.one_and_i_ins_one_out Circuittype.MUX)

  | prim "~"        = Maybe.success (PrimTypes.num_prim     ArithPrims.arithNeg)
  | prim "+"        = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithPlus)
  | prim "-"        = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithMinus)
  | prim "*"        = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithTimes)
  | prim "/"        = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithDivide)
  | prim "$max"     = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithMax)
  | prim "$min"     = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithMin)
  | prim "$mod"     = Maybe.success (PrimTypes.int_int_prim ArithPrims.arithMod)
  | prim "$gtn"     = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithGtn)
  | prim "$geq"     = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithGeq)
  | prim "$ltn"     = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithLtn)
  | prim "$leq"     = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithLeq)
  | prim "$eq"      = Maybe.success (PrimTypes.num_num_prim ArithPrims.arithEq)
  | prim "Not"      = Maybe.success (PrimTypes.int_prim     ArithPrims.arithNot)

  | prim "append"   = Maybe.success (PrimTypes.nat_nat_prim WiringPrims.append)

  | prim _          = Maybe.failure

(**************************************************************Circuittype.************)
(**************************************************************Circuittype.************)

end (* of structure CompileDef *);
(* open CompileDef *)
