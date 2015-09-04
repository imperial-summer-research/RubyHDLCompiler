(**************************************************************************)
(**************************************************************************)
(***                                                          ***)
(***    PrintMax: print as Maxeler MaxJ design                          ***)
(***    currently outputs Max v2 but code for v1 left as comments       ***)
(***                                                          ***)
(***    These functions give a printable string representation of a     ***)
(***    pcircuit.                                                       ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure PrintMax :
  sig
        (* global input / output numbers *)
        (* could use some foldl-based quasi-monadic nightmare instead *)
        val inputCount : int ref
        val outputCount : int ref


        (* note we pass command line twice: one parsed into options, the other
           a raw char list *)
        val showMax : string -> Circuittype.pcircuit -> CommandLineParse.Defs -> char list -> string
        val showMaxManager : string -> Circuittype.pcircuit -> CommandLineParse.Defs -> char list -> string
  end =
struct

(**************************************************************************)
(**************************************************************************)


(* global input / output numbers *)
(* could use some foldl-based quasi-monadic nightmare instead *)
val inputCount = ref 0;
val outputCount = ref 0;
val tmpVectorCount = ref 0;

(* utilities to simultaneously use and increment the input / output counters
 * this ensures each input or output name is unique, viz: in1, in2, etc.
*)
fun nextInputName () = (inputCount := 1 + !inputCount;
                        !inputCount)
fun nextOutputName () = (outputCount := 1 + !outputCount;
                        !outputCount)


(* emit max for node: 't' + node number as string *)
fun maxShowNode nodeNum =
    "t" ^ (Int.toString nodeNum)



val kernelMathImport = "com.maxeler.maxcompiler.v2.kernelcompiler.stdlib.KernelMath"


(* get imports from each device. Return list to allow multiple imports *)
fun 
    deviceImports Circuittype.EXP               = [kernelMathImport]
  | deviceImports Circuittype.LOG               = [kernelMathImport]
  | deviceImports Circuittype.MAX               = [kernelMathImport]
  | deviceImports Circuittype.MIN               = [kernelMathImport]
  | deviceImports Circuittype.ABS               = [kernelMathImport]
  | deviceImports (_)                           = []


(* imports from each gate *)
fun gateImports (device, _, _) = map (fn s => "import " ^ s ^ ";\n") (deviceImports device)


(* imports from each relation *)
fun relationImports ds = List.concat (map gateImports ds)
  

(* imports from each relations (pcircuit has list of relations) *)
fun relationsImports rs = List.concat (map relationImports rs)



(* print an expr. Temporary vars are simply node number prefixed with 't' *)
fun showExpr e = (case !e of
                    Circuittype.CON k              => Show.showConst k
                  | Circuittype.WIRE (Circuittype.POLY,name,_) => 
                        maxShowNode name (* use ":"? *)
                  | Circuittype.WIRE (_,   name,_) => maxShowNode name
                  | Circuittype.LIST es            => 
                        "" ^
                        Utilities.concatWith "," (map showExpr es) ^
                        ""
                  | Circuittype.EXPR x             => showExpr x
                 )



(* map device name to max device name;
   add extra imports to the set
   implemented as pattern-matching function rather than a table.
*)
fun 
    lookupMaxName (Circuittype.EXP) =   "KernelMath.pow" 
    | lookupMaxName (Circuittype.LOG) = "KernelMath.log" 
    | lookupMaxName (Circuittype.MAX) = "KernelMath.max" 
    | lookupMaxName (Circuittype.MIN) = "KernelMath.min" 
    | lookupMaxName (Circuittype.ABS) = "KernelMath.abs" 
    | lookupMaxName _ =
        Errors.simple_error "Internal error: MaxJ device not found"



(* translate Ruby primitive to Maxeler equivalent as string 
   note operators are translated to symbols, e.g. 'add' => '+'
*)
fun maxshowDevice Circuittype.NOTHING           = "**ERROR**: maxShowDevice: NOTHING should not occur!"
  | maxshowDevice Circuittype.D                 = "D ?"
  | maxshowDevice (Circuittype.DI k)            = "D "    ^ Show.showConst k
  | maxshowDevice (Circuittype.MUX n)           = "MUXR "  ^ Int.toString n
  | maxshowDevice (Circuittype.SDPR n)          = "sdpr " ^ Int.toString n
  | maxshowDevice (Circuittype.PDSR n)          = "pdsr " ^ Int.toString n
  | maxshowDevice Circuittype.NOT               = "~"
  (* Max uses & and | for logical operators *)
  | maxshowDevice Circuittype.AND               = "&"
  | maxshowDevice Circuittype.OR                = "|"
  | maxshowDevice Circuittype.XOR               = "^"
  | maxshowDevice Circuittype.NAND              = "nand"
  | maxshowDevice Circuittype.NOR               = "nor"
  | maxshowDevice Circuittype.XNOR              = "xnor"
  | maxshowDevice Circuittype.LT                = "<"
  | maxshowDevice Circuittype.GT                = ">"
  | maxshowDevice Circuittype.LE                = "<="
  | maxshowDevice Circuittype.GE                = ">="
  (* since maxj is a reference-based language, must emit '===' to compare
     values ('==' only compares references)
  *)
  | maxshowDevice Circuittype.EQ                = "==="
  (* IF is replaced by ?: below *)
  | maxshowDevice Circuittype.IF                = "?:"
  | maxshowDevice Circuittype.BTOI              = "bit2int"
  | maxshowDevice Circuittype.ITOB              = "int2bit"
  | maxshowDevice Circuittype.ADD               = "+"
  | maxshowDevice Circuittype.SUB               = "-"
  | maxshowDevice Circuittype.MULT              = "*"
  | maxshowDevice Circuittype.DIV               = "/"
  | maxshowDevice Circuittype.MOD               = "%"
  (* some operations implemented in KernelMath *)
  | maxshowDevice Circuittype.EXP               = lookupMaxName (Circuittype.EXP)
  | maxshowDevice Circuittype.LOG               = lookupMaxName (Circuittype.LOG)
  | maxshowDevice Circuittype.MAX               = lookupMaxName (Circuittype.MAX)
  | maxshowDevice Circuittype.MIN               = lookupMaxName (Circuittype.MIN)
  | maxshowDevice Circuittype.GCD               = "gcd"
  | maxshowDevice Circuittype.FAC               = "fac"
  | maxshowDevice Circuittype.DEC               = "dec"
  | maxshowDevice Circuittype.INC               = "inc"
  | maxshowDevice Circuittype.AD                = "AD"
  | maxshowDevice Circuittype.ABS               = lookupMaxName (Circuittype.ABS)
  | maxshowDevice Circuittype.REAL2INT          = "real2int"
  | maxshowDevice Circuittype.INT2REAL          = "int2real"
  | maxshowDevice (Circuittype.BIT2UINT n)      = "bit2uint "
  | maxshowDevice (Circuittype.BIT2SINT n)      = "bit2sint "
  | maxshowDevice (Circuittype.UINT2BIT n)      = "uint2bit " ^ Int.toString n
  | maxshowDevice (Circuittype.SINT2BIT n)      = "sint2bit " ^ Int.toString n
  | maxshowDevice (Circuittype.BIT2UREAL (m,n)) = "bit2ureal "
  | maxshowDevice (Circuittype.BIT2SREAL (m,n)) = "bit2sreal "
  | maxshowDevice (Circuittype.UREAL2BIT (m,n)) = "ureal2bit " ^ Int.toString m ^ " " ^ Int.toString n
  | maxshowDevice (Circuittype.SREAL2BIT (m,n)) = "sreal2bit " ^ Int.toString m ^ " " ^ Int.toString n
  | maxshowDevice (Circuittype.DEVICE s)        = s


(* isInfix: true ifCircuittype.f device is an infix operator in Max *)
fun 
    isInfix Circuittype.AND               = true
  | isInfix Circuittype.OR                = true
  | isInfix Circuittype.XOR               = true
  | isInfix Circuittype.NAND              = true
  | isInfix Circuittype.NOR               = true
  | isInfix Circuittype.XNOR              = true
  | isInfix Circuittype.LT                = true
  | isInfix Circuittype.GT                = true
  | isInfix Circuittype.LE                = true
  | isInfix Circuittype.GE                = true
  | isInfix Circuittype.EQ                = true
  | isInfix Circuittype.ADD               = true
  | isInfix Circuittype.SUB               = true
  | isInfix Circuittype.MULT              = true
  | isInfix Circuittype.DIV               = true
  | isInfix Circuittype.MOD               = true
  | isInfix _                             = false

(* isDelay: true iff device is a delay *)
fun isDelay Circuittype.D                 = true
  | isDelay (Circuittype.DI k)            = true
  | isDelay _                             = false

(* isAntiDelay: true iff device is a delay *)
fun isAntiDelay Circuittype.AD            = true
  | isAntiDelay _                         = false

fun isIf Circuittype.IF                   = true
  | isIf _                                = false

fun isEqOp Circuittype.EQ                 = true
  | isEqOp _                              = false


(** 
 * Now we have a new function type of circuit 
 * As we might need to concatenate nodes not only by simple infix/prefix operators
 *)
fun isFunc (Circuittype.PDSR n)           = true
  | isFunc (Circuittype.SDPR n)           = true
  | isFunc _                              = false


(* emit a Maxeler node definition: DFEVar (node) = (init); 
   put this in a separate function in case Maxeler change all the names again
*)
fun maxShowNodeDefinition nodeStr initStr =
    "   " ^
    "DFEVar " ^ nodeStr ^ " = " ^ initStr ^ ";\n"

(* emit Maxeler code to back patch a previously-emitted forward-declared node *)
fun maxBackPatchForwardDeclaration nodeStr initStr =
    "   " ^
    nodeStr ^ " <== " ^ initStr ^ ";\n"

(* emit max for stream offsets
   offset of exprStr by offsetStr
   also in separate function to give single point to change
 *)
fun maxShowStreamOffset exprStr offsetStr =
    "stream.offset(" ^  exprStr ^ ", " ^ offsetStr ^ ")"

fun showFuncParams input =
    case !input of
        Circuittype.LIST es => Utilities.concatWith "," (map showExpr es)
      | Circuittype.EXPR x  => showExpr x


fun maxShowFunc device input = 
    case device of
        (* just copy and paste, should have better solution *)
        (Circuittype.PDSR n) => "pdsr" ^ Int.toString n ^ "(" ^ showFuncParams input ^ ")"
      | (Circuittype.SDPR n) => "sdpr" ^ Int.toString n ^ "(" ^ showFuncParams input ^ ")"
      | _ => "unsupported function."



(* print gate as Maxeler DFG fragment 
   takes higher order function so we can customize output
 *)
(* vincent 2015.08 add support for multiple output *)
fun showGate (showNodeFunc) (device, input, output) = 
  (showNodeFunc)
  (* the declaration part of the output *)
  (showExpr output)
  (
    (* infix ops: generate by interspersing translated operator
    within its inputs, so:
    add <.2,.3> ~ .1 => DFEVar t1 = t2 + t3;
    but note that eq should translate to method calls, so
    eq <2., .3} ~ .1 => DFEVar t1 = t2.eq(t3);
    because == requires references in MaxJ, not values.
    Note there's no neq operator (block) in Ruby.
    *)
    if isEqOp device
    then (
        case !input of
            Circuittype.CON k       => "ERROR_EQ_CONSTANT"
          | Circuittype.WIRE (_,   name,_) => "ERROR_EQ_WIRE"
          | Circuittype.LIST es            => 
                   (
                      "(" ^ (showExpr (List.nth(es, 0))) ^ ").eq" ^
                      "(" ^ (showExpr (List.nth(es, 1))) ^ ")"
                   )
          | Circuittype.EXPR x             => "ERROR_EQ_EXPR"
    )
    else if isFunc device 
    then (
        maxShowFunc device input
    )
    else if isInfix device
    then (
        case !input of
            Circuittype.CON k              => Show.showConst k
          | Circuittype.WIRE (Circuittype.POLY,name,_) => 
                maxShowNode name (* use ":"? *)
          | Circuittype.WIRE (_,   name,_) => maxShowNode name
          | Circuittype.LIST es            => 
                Utilities.concatWith (maxshowDevice device) (map showExpr es)
          | Circuittype.EXPR x             => showExpr x
    )
    (* translate delays to stream -ve offsets *)
    else if isDelay device
    then (
        maxShowStreamOffset (showExpr input) "-1"
    )
    (* translate antidelays to stream +ve offsets *)
    else if isAntiDelay device
    then (
        maxShowStreamOffset (showExpr input) "1"
    )
    (* IF to conditional operator (?:) *)
    else if isIf device
    then (
        "(" ^
        (
        (* hack: only support input which is a list of expressions
        *        if doesn't make much sense otherwise *)
        case !input of
                  Circuittype.CON k       => "ERROR_IF_CONSTANT"
                | Circuittype.WIRE (_,   name,_) => "ERROR_IF_WIRE"
                | Circuittype.LIST es            =>
                   (
                      "(" ^ (showExpr (List.nth(es, 0))) ^ ") ? " ^
                      "(" ^ (showExpr (List.nth(es, 1))) ^ ") : " ^
                      "(" ^ (showExpr (List.nth(es, 2))) ^ ")"
                   )
                | Circuittype.EXPR x             => "ERROR_IF_EXPR"
        ) ^
        ")"
    )
    else (
        (* noninfix: just translate brackets *)
        Utilities.ljustify 15 (maxshowDevice device) ^
        "(" ^
        Utilities.ljustify 19 (showExpr input) ^
        ")" 
    )
  )

(* return list of nodes from expression 
   i.e. remove constants
 *)
fun exprNodes expr =
    case !expr of
          Circuittype.CON k              => []
        | Circuittype.WIRE (_,   name,_) => [name]
        | Circuittype.LIST es            =>
           (
            List.concat (map exprNodes es)
           )
        | Circuittype.EXPR e             => exprNodes e


(* maxj fragment for forward declaration of node *)
val maxEmitForwardDeclareNode = "dataType.newInstance(this)"


(* quasi-monadic nightmare to emit forward decl if node is undeclared *)
fun forwardDeclareIfUndefined (nodeNum, (defs, inStr, inForwardDefs)) =
    let val nodeNumStr = maxShowNode nodeNum
    in
    case (List.find (fn d => d = nodeNum) defs) of
        SOME _ => 
            (* found => already defined: do nothing *)
            (defs, inStr, inForwardDefs)
        | NONE => 
            (* not found => emit forward def. add to list *)
            (
            defs, 
            inStr ^ 
      (maxShowNodeDefinition nodeNumStr maxEmitForwardDeclareNode), 
            nodeNum::inForwardDefs
            )
    end

fun isCyclicLoop(inNodes, outp) = 
    List.foldl (fn (x, b) => ((x > outp) orelse b)) true inNodes

fun pushPipelineIfCyclic (inNodes, outp) = 
    if isCyclicLoop(inNodes, outp) then "\toptimization.pushPipeliningFactor(0);\n" else ""

fun popPipelineIfCyclic (inNodes, outp) = 
    if isCyclicLoop(inNodes, outp) then "\toptimization.popPipeliningFactor();\n" else ""

(* emit node definition with forward declarations for any nodes not yet defined *)
(* quasi-monadic nightmare for use with foldr / foldl *)
(* takes curr device and triple (defs, string, forward defs), returns updated (defs, string, updated defs)*)
fun showGateWithForwardDeclarations ((device, input, output), (inDefs, inStr, inForwardDefs)) =
  (* gather lists of input and output nodes *)
  let 
    val inNodes   = exprNodes input
    val outNodes  = exprNodes output
    (* build string of any forward definitions *)
    val (_, forwardDefsStr, forwardDefs2) = List.foldl forwardDeclareIfUndefined (inDefs, "", inForwardDefs) inNodes
  in
    case outNodes of
      [] => (
        Errors.simple_error "Maxj: all devices must have one output (0)"
      )
    | [outp] => (
      case List.find (fn x => x = outp) inForwardDefs of
        SOME _ => 
          (
            outp::inDefs,
            forwardDefsStr ^
            inStr ^ 
            (pushPipelineIfCyclic (inNodes, outp)) ^
            (showGate (maxBackPatchForwardDeclaration) (device, input, output)) ^
            (popPipelineIfCyclic (inNodes, outp)),
            (* remove from forward defs *)
            Utilities.snd (List.partition (fn x => x = outp) forwardDefs2)
          )
        | NONE =>
          (
            outp::inDefs,
            forwardDefsStr ^ inStr ^ (showGate (maxShowNodeDefinition) (device, input, output)),
            forwardDefs2
          )
      )
     | _ => ( Errors.simple_error "Maxj: all devices must have one output (n)" )
  end


(* show relation (no forward defs) *)
fun showRelation ds = 
    Utilities.concatWith "" (map (showGate maxShowNodeDefinition) ds)

val relationsSep =      "   //----------------------------------------\n"

(* show relation (WITH forward defs) *)
fun showRelationFwd (ds, (inDefs, inStr, inForwardDefs)) = 
    (*
    Utilities.concatWith "" (map (showGate maxShowNodeDefinition) ds)
    *)
    let val (outDefs, outStr, outForwardDefs) =
        List.foldl showGateWithForwardDeclarations (inDefs, inStr, inForwardDefs) ds
    in
        (
            outDefs,
            outStr ^ relationsSep,
            outForwardDefs
        )
    end


val relationsPreamble = 
                        " // Relations:\n" ^
                        "\n   // Output           Input(s)\n"

(* show list of ordered lists of relations *)
fun showRelations [] = ""
  | showRelations rs = (
                            relationsPreamble ^
                            relationsSep ^
                            Utilities.concatWith relationsSep (map showRelation rs) ^
                            relationsSep
                        )

(* show ordered relations including forward declarations *)
fun showRelationsFwd rs inDefs =
    let val (outDefs, outStr, outForwardDefs) = 
        List.foldl showRelationFwd ((*inDefs*)inDefs, "", (*inFwdDefs*)[]) rs
    in
    (
        (* should maybe check no forward defs remain *)
        relationsPreamble ^
        relationsSep ^
        outStr ^
        relationsSep
    )
    end

fun showWiring (dom, ran) = "\n // Wiring -  " ^
                            showExpr dom ^ " ~ " ^ showExpr ran ^ "\n"

(* declare input *)
fun showInput e = 
    maxShowNodeDefinition 
        (*nodeStr= *) 
            (case !e of
                    Circuittype.CON k       => "/*con*/ " ^ Show.showConst k
                  | Circuittype.WIRE (Circuittype.POLY,name,_) => 
                        "/*poly*/ " ^ maxShowNode name (* use ":"? *)
                  | Circuittype.WIRE (_,   name,_) => "/*_*/ " ^ maxShowNode name
                  | Circuittype.LIST es            => 
                        "<" ^
                            Utilities.concatWith "," (map showInput es) ^
                        ">"
                  | Circuittype.EXPR x             => showInput x
                 )
        (* initStr= *)
            (
             "io.input(\"inp" ^
             (* use and increment the input counter *)
             (Int.toString (nextInputName())) ^
             "\", dataType)"
             )


(* isInputDir: true iff input is IN or POLY (bidirectional, which we interpret
as input) *)
fun isInputDir dir = (case dir of
                        Circuittype.IN     => true
                      | Circuittype.OUT    => false
                      | Circuittype.POLY   => true
                      )


(* isInput: true iff wire is an input *)
(* for some reason you can't have more than one level of pattern match here.
   If we wrote this function like:
   ...
            | Circuittype.WIRE (IN  ,_,_)  => true
            | Circuittype.WIRE (OUT  ,_,_)  => false
            | Circuittype.WIRE (POLY  ,_,_)  => true
    ...
    then SMLNJ gives an error: "redundant match" between OUT and POLY cases,
    even though they should be different in any sane language

    Note we exhaustively match to prevent any idiotic warnings about
    inexhaustive matches.
*)
fun isInput e = (case !e of
              Circuittype.CON _              => false
            | Circuittype.WIRE (dir  ,_,_)  => isInputDir dir
            | Circuittype.LIST _  => false
            | Circuittype.EXPR _  => false
           )

(* gather inputs from both domain and range *)
fun circuitInputs (dom, ran)
        
        = let  
               val wires = Exprs.exprWires dom @ Exprs.exprWires ran
               val inputs = Utilities.filter isInput wires
               val ins = if    null inputs
                           then  []
                           else  List.concat (map exprNodes inputs)
          in   
            ins
          end

(* print inputs from both domain and range in max format *)
fun showInputs (dom, ran)
        
        = let  
        (*
               fun isInput e = (case !e of
                                  Circuittype.CON _              => false
                                | Circuittype.WIRE (IN  ,_,_)  => true
                                | Circuittype.WIRE (POLY,_,_)  => true
                                | Circuittype.WIRE (OUT,_,_)  => false
                                | Circuittype.LIST _  => false
                                | Circuittype.EXPR _  => false
                               )
        *)
                               
               val wires = Exprs.exprWires dom @ Exprs.exprWires ran
               val inputs = Utilities.filter isInput wires
               val inStr = if    null inputs
                           then  "// (none)\n"
                           else  Utilities.concatWith "\n" (map showInput inputs)
          in   "\n // Inputs -  \n" ^ inStr ^ "\n"                
          end

fun showDirections (dom, ran)
        = let  fun showDir e = (case !e of
                                  Circuittype.CON k           => "out1"
                                | Circuittype.WIRE (Circuittype.IN,  _,_) => "in"
                                | Circuittype.WIRE (Circuittype.OUT, _,_) => "out2"
                                | Circuittype.WIRE (Circuittype.POLY,_,_) => "?"
                                | Circuittype.LIST es         => "<" ^ Utilities.concatWith ","
                                                           (map showDir es) ^
                                                  ">"
                                | Circuittype.EXPR x          => showDir x
                               )
          in   "\n // Directions -  " ^ showDir dom ^ " ~ " ^ showDir ran ^ "\n"
          end

(* declare output *)
fun showOutput e = 
            "      io.output(\"out" ^
            Int.toString (nextOutputName()) ^
            "\", " ^
            (case !e of
                    Circuittype.CON k       => Show.showConst k
                  | Circuittype.WIRE (Circuittype.POLY,name,_) => 
                        "/*poly*/ " ^ maxShowNode name (* use ":"? *)
                  | Circuittype.WIRE (_,   name,_) => "/*_*/ " ^ maxShowNode name
                  | Circuittype.LIST es            => 
                        "<" ^
                            Utilities.concatWith "," (map showOutput es) ^
                        ">"
                  | Circuittype.EXPR x             => showOutput x
                 ) ^
             ", dataType);"

(* print outputs 
   each output becomes an io.output call
   e.g. .9 => io.output("out0", t9, dataType);
*)
fun showOutputs (dom, ran)
        
        = let  fun isOutput e = (case !e of
                                  Circuittype.WIRE (Circuittype.IN  ,_,_)  => false
                                | Circuittype.WIRE (Circuittype.POLY,_,_)  => true
                                | Circuittype.WIRE (Out,_,_)  => true
                                | _                => false
                               )
                               
               val wires = Exprs.exprWires dom @ Exprs.exprWires ran
               val outputs = Utilities.filter isOutput wires
               val outStr = if    null outputs
                           then  "// (none)"
                           else  Utilities.concatWith "\n" (map showOutput outputs)
          in   "\n // Outputs -  \n" ^ outStr ^ "\n"                
          end

(* showOutputsX: alternative to showOutputs which works on expressions directly.
   used to produce no output sometimes, probably due to unmatched pattern
   somewhere. Fixed now?

   Not used at present.
   *)
fun showOutputsX e = (case !e of
                    Circuittype.CON k       => 
                     "      io.output(\"out" ^
                     Int.toString (nextOutputName()) ^ "\", " ^
                     Show.showConst k ^
                     ", dataType); // (con)"
                  | Circuittype.WIRE (Circuittype.POLY,name,_) => 
                     "      io.output(\"out" ^
                     Int.toString (nextOutputName()) ^
                     "\", t" ^ Int.toString name ^
                     ", dataType); // (poly)"
                  | Circuittype.WIRE (Circuittype.OUT,   name,_) => 
                     "      io.output(\"out" ^
                     Int.toString (nextOutputName()) ^
                     "\", " ^ maxShowNode name ^
                     ", dataType); // (out)"
                  | Circuittype.WIRE (Circuittype.IN,   _,_) => 
                     ""    (* this is just to prevent warnings about
                     inexhaustive match *)
                  | Circuittype.LIST es            => "" ^
                                          Utilities.concatWith "\n" (map showOutputsX es) ^
                                          ""
                  | Circuittype.EXPR x             => showOutputsX x
                 )


(* output banner: show what was generated and command line *)
fun outputBanner currentName spacedCommandLine =
        "////////////////////////////////////////////////////////////////////\n" ^
        "// Generated by Imperial Ruby compiler\n" ^
        "// Maxeler MaxJ kernel output\n" ^
        "// command line: '" ^
        (implode spacedCommandLine) ^ "'\n" ^
        "// top-level block: '" ^ currentName ^ "'\n" ^
        "////////////////////////////////////////////////////////////////////\n"

(* standard Maxeler imports, needed for all designs
*)
val stdMaxImports =
        "import com.maxeler.maxcompiler.v2.kernelcompiler.Kernel;\n" ^
        "import com.maxeler.maxcompiler.v2.kernelcompiler.KernelParameters;\n"^
        "import com.maxeler.maxcompiler.v2.kernelcompiler.types.base.DFEVar;\n"^
        (* DFEType needed for parameterisable dataType *)
        "import com.maxeler.maxcompiler.v2.kernelcompiler.types.base.DFEType;\n" ^
        (* For cyclic loop issue *)
        "import com.maxeler.maxcompiler.v2.kernelcompiler.KernelConfiguration;\n"

(* if an option has been set, show its first argument, else show the default *)
fun showOptionFirstArgOrDefault  commandLineOptions optionName defaultValue =
  (case CommandLineParse.assocLookup optionName commandLineOptions of
     Option.SOME value =>
        (case value of
           (CLArgKindStruct.CLArgOptionArgs []) => "ERROR: option " ^
            optionName ^ "has no argument"
            (* select first argument from list *)
           | CLArgKindStruct.CLArgOptionArgs (n::ns) => n
           | CLArgKindStruct.CLArgOptionNoArg => "ERROR: " ^ optionName ^ " must have argument"
           (* last should be impossible unless program logic is faulty *)
           | CLArgKindStruct.CLArgPlain => "ERROR: " ^ optionName ^ " must be an option"
        )
        (* option not set => use default value *)
     | Option.NONE => defaultValue
  )


(* if an option has been set, show all arguments, else show the default *)
fun showOptionArgsOrDefault  
        commandLineOptions optionName defaultValue separator =
  (case CommandLineParse.assocLookup optionName commandLineOptions of
     Option.SOME value =>
        (case value of
            (* arguments from list *)
             CLArgKindStruct.CLArgOptionArgs (args) =>
                (concat o CommandLineParse.intersperse separator) args
           | CLArgKindStruct.CLArgOptionNoArg => "ERROR: " ^ optionName ^ " must have argument"
           (* last should be impossible unless program logic is faulty *)
           | CLArgKindStruct.CLArgPlain => "ERROR: " ^ optionName ^ " must be an option"
        )
        (* option not set => use default value *)
     | Option.NONE => defaultValue
  )

(* default values for user options *)
val defaultPackageName = "myapp_pkg"
val defaultDataType    = "dfeFloat(8, 24)"
val defaultPreamble    = ""
val defaultPostamble   = ""
val defaultUserImports = ""

(* MaxJ backend option names: all should start with '--maxj' 
   (not checked)
 *)
val optionSetPackage   = "--maxj-set-package"
val optionSetDataType  = "--maxj-set-datatype"
val optionAddPreamble  = "--maxj-add-preamble"
val optionAddPostamble = "--maxj-add-postamble"
val optionAddUserImport = "--maxj-add-user-import"

(* MaxJ builtin functions for Ruby primitives *)

fun showFuncDummyParams n prefix = 
    Utilities.concatWith "," (List.tabulate(n, fn x => prefix ^ " t" ^ Int.toString x))

fun showMaxPDSRDef n = 
    "\tprivate DFEVar pdsr" ^ Int.toString n ^ "(" ^ showFuncDummyParams n "DFEVar" ^ ") {\n" ^
    "\t  DFEVar counter = control.count.simpleCounter(MathUtils.bitsToAddress(" ^ Int.toString n ^ "));\n" ^
    "\t  return control.mux(counter," ^ showFuncDummyParams n "" ^ ");\n" ^
    "\t}\n"

fun showMaxSDPRDef n = 
    "\tprivate DFEVar sdpr" ^ Int.toString n ^ "(" ^ showFuncDummyParams n "DFEVar" ^ ") {\n" ^
    "\t  DFEVar counter = control.count.simpleCounter(MathUtils.bitsToAddress(" ^ Int.toString n ^ "));\n" ^
    "\t  return control.mux(counter," ^ showFuncDummyParams n "" ^ ");\n" ^
    "\t}\n"

(* hardcoded function *)
fun showMaxBuiltInDefs funcs = 
    (showMaxPDSRDef 2) ^
    (showMaxSDPRDef 2)

(* top-level: print design as a Maxeler MaxJ kernel *)
fun showMax currentName (dom, ran, rels)   commandLineOptions
              spacedCommandLine
        = 
        let val circuitInputNodes = circuitInputs (dom, ran)
            val currentKernelName = currentName ^ "Kernel"
        in
          (outputBanner currentName spacedCommandLine) ^
          "//\n" ^
          (* "package " ^ *)
          (* (showOptionFirstArgOrDefault commandLineOptions optionSetPackage defaultPackageName) ^ *)
             ";\n" ^
          "\n" ^
          "// Imports:\n" ^
          "// standard imports:\n" ^
          stdMaxImports ^
          "\n" ^
          (* non-standard inputs needed by operators, e.g. KernelMath.max *)
          "// extra imports:\n" ^
          (* use insertSet to remove duplicates *)
          (String.concat (foldr (fn (x, y) => Utilities.insertSet y x) [] (relationsImports rels) ) ) ^
          "\n" ^
          "// user imports:\n" ^
          (showOptionArgsOrDefault commandLineOptions optionAddUserImport defaultUserImports "\n") ^
          "\n" ^
          "// class:\n" ^
          "public class " ^ currentKernelName ^ " extends Kernel {\n" ^
          "\n" ^
          "  final DFEType dataType = " ^
          (showOptionFirstArgOrDefault commandLineOptions optionSetDataType defaultDataType) ^
          ";\n" ^
          "\n" ^
          "  public " ^ currentKernelName ^ " (KernelParameters parameters) {\n" ^
          "    super(parameters);\n" ^
          "\n" ^
          "\n" ^
          (* user-defined preamble, e.g. push DSP factor*)
          "// begin user preamble{\n" ^
          (showOptionArgsOrDefault commandLineOptions optionAddPreamble defaultPreamble "\n") ^
          "// } end user preamble\n" ^

          showInputs (dom, ran) ^

          (* main circuit *)
          (*
          showRelations (Utilities.filter (not o null) rels) ^
          *)
          (showRelationsFwd 
            (Utilities.filter (not o null) rels) circuitInputNodes ) ^

          showDirections (dom, ran) ^
          showWiring (dom, ran) ^
          showOutputs (dom, ran) ^

          (* showOutputsX: COMMENTED OUT to avoid duplication
          (*
          (showOutputsX dom) ^
          *)
          "\n" ^
          (showOutputsX ran) ^
          "\n" ^
          *)

          (* user-defined postamble, e.g. pop DSP factor*)
          "// begin user postamble{\n" ^
          (showOptionArgsOrDefault commandLineOptions optionAddPostamble defaultPostamble "\n") ^
          "// } end user postamble\n" ^
          "\n" ^
          "\n" ^
          "  } // end constructor\n" ^
          (showMaxBuiltInDefs []) ^
          "} // end class\n" ^
        "////////////////////////////////////////////////////////////////////\n"
        end

val stdMaxManagerImports =
        "import com.maxeler.maxcompiler.v2.build.EngineParameters;\n" ^
        "import com.maxeler.maxcompiler.v2.managers.custom.CustomManager;\n" ^
        "import com.maxeler.maxcompiler.v2.managers.custom.DFELink;\n" ^
        "import com.maxeler.maxcompiler.v2.managers.custom.blocks.KernelBlock;\n" ^
        "import com.maxeler.maxcompiler.v2.managers.engine_interfaces.CPUTypes;\n" ^
        "import com.maxeler.maxcompiler.v2.managers.engine_interfaces.EngineInterface;\n" ^
        "import com.maxeler.maxcompiler.v2.managers.engine_interfaces.InterfaceParam;\n"

fun showMaxManager currentName (dom, ran, rels)   commandLineOptions
              spacedCommandLine
    = 
    let val circuitInputNodes = circuitInputs (dom, ran)
        val currentManagerName = currentName ^ "Manager"
    in
        (outputBanner currentName spacedCommandLine) ^ "\n" ^
        "//\n" ^
        "// Imports:\n" ^
        "// standard imports:\n" ^
        stdMaxManagerImports ^
        "\n" ^
        "// class:\n" ^
        "public class " ^ currentManagerName ^ " extends CustomManager {\n" ^
        "\n" ^
        "  public " ^ currentManagerName ^ " (EngineParameters params) {\n" ^
        "    super(params);\n" ^
        (* KernelBlock definition kernel's name should be currentKernel *)
        "    KernelBlock" ^
        "\n" ^
        "  } // end constructor\n" ^
        (* EngineInterface definition *)
        "  public static EngineInterface interfaceDefault() {\n" ^
        
        "  }\n" ^
        "} // end class\n" ^
        "////////////////////////////////////////////////////////////////////\n"
    end
(**************************************************************************)
(**************************************************************************)

end (* of structure PrintMax *);
(* open PrintMax *)
