

structure Maxtype = 
struct
  datatype gate = 
    (* [id, input vector, output vector, name ] *)
    GATE of int * int list * int list * string
  datatype 
end

structure PrintMax : 
  sig
  	val showMax      : string -> Circuittype.pcircuit -> CommandLineParse.Defs -> char list -> string
    
    (* helper functions *)
    val exprToIdList : Circuittype.expr -> int list
    
    (* Processors *)
    (* val procGate : Circuittype.gate -> Maxtype.gate list -> Maxtype.gate list *)
  end =
struct

	fun showExpr e = 
    case !e of
      Circuittype.CON k              => Show.showConst k
    | Circuittype.WIRE (_, id, ends) => "NODE[" ^ Int.toString id ^ "][" ^ Int.toString ends ^ "]"
    | Circuittype.LIST es            => "" ^ Utilities.concatWith "," (map showExpr es) ^ ""
    | Circuittype.EXPR x             => showExpr x

  fun exprToIdList e =
    case !e of
      Circuittype.WIRE (_, id, ends) => ((print "WIRE\t"); [id])
    | Circuittype.LIST es            => ((print "LIST\t"); List.foldl (fn (e,r) => (exprToIdList e)@r) [] es)
    | Circuittype.EXPR x             => ((print "EXPR\t"); exprToIdList x)
    | _                              => Errors.simple_error (showExpr e)

  fun procGate ((device, input, output), gs) =
    let 
      val id        = List.length gs + 1
      val inIdList  = exprToIdList input
      val outIdList = exprToIdList output
      val name      = Show.showDevice device
    in
      ( print 
        ( "Gate [" ^ name ^ "]:\n" ^
          "Input number:\t "  ^ (Int.toString (List.length inIdList )) ^ "\n" ^
          "Output number:\t " ^ (Int.toString (List.length outIdList)) ^ "\n"
        );
      (Maxtype.GATE (id, inIdList, outIdList, name))::gs)
    end

  (** 
   * procRel: Process relations
   * Each realation in ruby contains many gates, and all these gates share one same name 
   *)
  fun procRel (rel, rs) =
    let 
      val gs = List.foldl procGate [] rel
    in
      print ("Relation has " ^ (Int.toString (List.length gs)) ^ " number of gates\n");
      gs@rs
    end

  val scalarType = "dfeFloat(8, 24)"

  fun showMaxDFEVectorDecl (scalarType, size) =
    "new DFEVectorType(" ^ scalarType ^ ", " ^ Int.toString size ^ ").newInstance(this)"

  fun showMaxGateDecl (Maxtype.GATE (id, inList, outList, name)) = 
    let 
      val gateNamePrefix  = name ^ Int.toString id
      val gateVarType     = "DFEVector<DFEVar>" 
      val gateInVarName   = gateNamePrefix ^ "_" ^ "IN"
      val gateOutVarName  = gateNamePrefix ^ "_" ^ "OUT"
      val gateInSize      = List.length inList
      val gateOutSize     = List.length outList
    in 
      gateVarType ^ " " ^ gateInVarName  ^ "\t = " ^ showMaxDFEVectorDecl (scalarType, gateInSize ) ^ ";\n" ^
      gateVarType ^ " " ^ gateOutVarName ^ "\t = " ^ showMaxDFEVectorDecl (scalarType, gateOutSize) ^ ";\n" ^
      gateOutVarName ^ "\t <== " ^ name ^ "(" ^ gateInVarName ^ ")" ^ ";\n"
    end

  fun showMaxGateConns (Maxtype.GATE (id, inList, outList, name)) =
    let 
      val gateNamePrefix = name 
      (* Get input conns *)

    in
      ""
    end

  fun showMaxRels maxRels = 
    let 
      val gateDecls = List.foldl (fn (x,s) => s ^ (showMaxGateDecl x)) "" maxRels
      val gateConns = ""
    in
      gateDecls ^ gateConns
    end

	fun showMax currentName (dom, ran, rels) commandLineOptions spacedCommandLine
		=
    let 
      (* First iterate all the rels *)
      val maxRels = List.foldl procRel [] rels;
    in
      (showMaxRels maxRels) ^ "Hello World"
    end
end
