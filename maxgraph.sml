
(*
* MaxGraph: Build and split DAG of ruby circuits.
* Author: Vincent Zhao
* Date: 2015.09
*   
* This file includes a MaxGraph structure, which will absorb an original Ruby
* "meta-data": pcircuit. After this, MaxGraph will construct a simple graph
* and perform several operations upon it, such like splitting. At last, it will 
* generate a list of pcircuits, they will be used to print MaxJ kernel file.
* It will also take care of printing MaxJ Manager.
**)

structure MaxGraph :
  sig

  	type edge
    type node
    type graph

  	(**
  	 * buildGraph: from ruby pcircuite generate a graph.
  	 *)
 		val buildGraph : Circuittype.pcircuit -> unit
  end =
struct 

	(**
	 * Edge: the edge in this DAG graph, 4 components:
	 * 1. FROM node id: the id that output to this edge
	 * 2. TO   node id: the id that this edge input to
	 * 3. RUBY wire id: each edge in this GRAPH is a ruby wire.
	 * 4. CLOCK label:  the most important part of this graph, we need 
	 *		to change clock rate between each node. Use real to represent
	 **)

	datatype edge = 
		Edge 			of int option * int option * int * real option
	| ConstEdge of Rubytype.const (* will do nothing on this const *) 

	fun showEdgeNode NONE 		= "None   "
		| showEdgeNode (SOME i)= "Node[" ^ Int.toString i ^ "]"

	fun showEdge (Edge (from, to, id, clock)) =
			"EDGE: from " ^ showEdgeNode from ^ 
			" to " ^ showEdgeNode to ^ 
			" id " ^ Int.toString id ^ 
			" clock " ^  (if (isSome clock) then Real.toString (valOf clock) else "UNSET") 
		| showEdge (ConstEdge v) =
			"CONST: " ^ Show.showConst v

  fun showEdges es = List.foldl (fn (x,s) => s ^ "\n" ^ (showEdge x)) "" es

	fun exprToEdges (e: Circuittype.expr) =
		case !e of
			Circuittype.CON v 				=> [ConstEdge v]
		| Circuittype.WIRE (_,id,_)	=> [Edge (NONE, NONE, id, NONE)] (* the clock rate will be initialized as NONE *)
		| Circuittype.LIST es 			=> List.foldr (fn (x,s) => (exprToEdges x)@s) [] es
		| Circuittype.EXPR e 				=> exprToEdges e

  fun eqEdge id (Edge (_,_,x,_)) = x = id
    | eqEdge _ _ = false

  (**
   * appendEdgeToEdgeList: append an edge to a list of edges, return the index of edge
   * If this param edge is a const, just append it.
   * If this param edge is an edge, first try to find whether this edge is in the list(by using eqEdge)
   *  if exists, return the id, don't change the list. If not, append it.
   **)
  fun appendEdgeToEdgeList edge edgeList =
    let 
      val size = List.length edgeList
    in
    ( case edge of
        ConstEdge _     => (edgeList@[(size, ref edge)], size)
      | Edge (_,_,id,_) =>
      ( let
          val findEdge = List.find (fn (_,e) => eqEdge id (!e)) edgeList
        in 
        ( case findEdge of
            NONE      => (edgeList@[(size, ref edge)],size)
          | SOME(x,_) => ((edgeList, x)))
        end ))
    end

  (**
   * appendEdgesToEdgeList: append a list of edges to the edge list, return a list of indices
   **)
  fun appendEdgesToEdgeList []      edgeList = (edgeList, [])
    | appendEdgesToEdgeList (e::es) edgeList =
    ( let
        val (nEdgeList, edgeId)     = appendEdgeToEdgeList  e  edgeList
        val (fEdgeList, edgeIdList) = appendEdgesToEdgeList es nEdgeList
      in 
        (fEdgeList, edgeId::edgeIdList)
      end)

  fun findEdgeById id eL = 
    let 
      val edge = List.find (fn (x,_) => id = x) eL 
    in
    ( case edge of 
        NONE      => (Errors.simple_error "ERROR: findEdgeById - Can't find edge")
      | SOME(x,e) => e)
    end

  fun getEdgeToNodeId (Edge (_,t,_,_)) = t
    | getEdgeToNodeId _ = (Errors.simple_error "ERROR: getEdgeToNodeId - Not common Edge type")

  fun showEdgeList []           = ""
    | showEdgeList ((id,e)::es) =
    ( let 
        val curEdgeStr = "ID: " ^ Int.toString id ^ " " ^ showEdge (!e)
        val fullStr = curEdgeStr ^ "\n" ^ (showEdgeList es)
      in
        fullStr
      end )

  (**
   * Node: the node in the DAG graph, has 3 components:
   * 1. FROM edge id list: the input edges to this node
   * 2. TO   edge id list: the output edges of this node
   * 3. DEVICE: ruby original device info
   **)

  datatype node =
    Node of int list * int list * Circuittype.device

  fun findNodeById nodeId nL = 
    let
      val node = List.find (fn (x,_) => x = nodeId) nL
    in
    ( case node of
        NONE      => (Errors.simple_error "ERROR: findNodeById - Can't find node")
      | SOME(x,n) => n )
    end

  fun getNodeOutEdges (Node (_,es,_)) = es

  (** 
   * This is an ad-hoc solution: we just check the device type of node,
   * if it's pdsr, we times n;
   * if it's sdpr, we div n;
   * else, just remain the original value.
   **)
  fun getNodeClockScale (Node (_,_,Circuittype.PDSR n)) = Real.fromInt n
    | getNodeClockScale (Node (_,_,Circuittype.SDPR n)) = 1.0 / (Real.fromInt n)
    | getNodeClockScale _ = 1.0

  fun showNode (Node (l1,l2,device)) = 
    let 
      val l1Str = Utilities.concatWith "," (List.map (Int.toString) l1)
      val l2Str = Utilities.concatWith "," (List.map (Int.toString) l2)
      val devStr= Show.showDevice device
    in
      "Device: " ^ devStr ^ "\t Input: [" ^ l1Str ^ "]\t Output: [" ^ l2Str ^ "]"
    end

  fun showNodeList []           = ""
    | showNodeList ((id,n)::ns) =
    ( let 
        val curNodeStr = "ID: " ^ Int.toString id ^ " " ^ showNode (!n)
      in 
        curNodeStr ^ "\n" ^ (showNodeList ns)
      end ) 

  (** Graph **)
  type graph = (int * node ref) list * (int * edge ref) list

  (** Graph setters **)
  fun setGraphEdgeInOutNode edgeId from to (_, eL) =
    let
      val e = findEdgeById edgeId eL
    in 
    ( case !e of
        Edge (f,t,i,clk) => (
          (case from of SOME(x) => e := Edge (from,t,i,clk) | _ => ());
          (case to   of SOME(x) => e := Edge (f,to,i,clk)   | _ => ())
        )
      | _ => () )
    end

  fun setGraphEdgeClock edgeId clk (_, eL) =
    let
      val e = findEdgeById edgeId eL
    in
    ( case !e of
        Edge (f,t,i,NONE) => e := Edge (f,t,i,(SOME clk))
      | _ => () )
    end

  (** Graph getters **)
  fun getGraphInputEdges (_,[])             = []
    | getGraphInputEdges (nL,((x,e)::es))   = 
    ( case !e of 
        (Edge (NONE,_,_,_)) => (getGraphInputEdges (nL,es))@[x]
      | _                   => (getGraphInputEdges (nL,es)) )


  (**
   * initGraphEdgeClock: start from an edge, set it's clock and scale the clock by
   * its output node device type.
   * If the output is an empty list, just stop.
   **)
  fun initGraphEdgeClock edgeId clk ((nL,eL):graph) = 
    let
      val e   = findEdgeById edgeId eL
      val nn  = getEdgeToNodeId (!e)
    in
      setGraphEdgeClock edgeId clk (nL,eL);
      ( case nn of
          NONE    => ()
        | SOME(x) => 
        ( let 
            val n = findNodeById x nL
            val nextClk = clk * (getNodeClockScale (!n))
          in
            List.app 
              (fn (x) => (initGraphEdgeClock x nextClk (nL, eL))) 
              (getNodeOutEdges (!n)) 
          end ))
    end

  fun initGraphClock clk g = 
    let 
      val inEdgeIds = getGraphInputEdges g
    in
      List.app (fn (x) => (initGraphEdgeClock x clk g)) inEdgeIds
    end

  (**
   * appendGateToGraph: append a gate to a graph.
   * Please notice that there's no function which could append a node to the nodelist
   * as the node has to know about the current context, it has the index of edge in the
   * edge list.
   **)
  fun appendGateToGraph (device, inExpr, outExpr) (nL, eL) =
    let
      val (eL1, iL1) = appendEdgesToEdgeList (exprToEdges inExpr ) eL
      val (eL2, iL2) = appendEdgesToEdgeList (exprToEdges outExpr) eL1
      val node       = Node (iL1, iL2, device)
      val nLLen      = List.length nL
      val nG         = (nL@[(nLLen,ref node)], eL2)
    in
      List.app (fn (x) => setGraphEdgeInOutNode x (SOME nLLen) NONE nG) iL2; (* set output edges' source *)
      List.app (fn (x) => setGraphEdgeInOutNode x NONE (SOME nLLen) nG) iL1; (* set input edges' destination *)
      nG
    end

  fun appendRelsToGraph []        g = g
    | appendRelsToGraph (gs::rs)  g =
    ( let
        val g1 = List.foldl (fn (g,gh) => appendGateToGraph g gh) g gs
      in 
        appendRelsToGraph rs g1
      end )

  (**
   * Graph Split:
   * The idea is quite simple. From the input nodes of the graph, and then iterate
   **)

  (* fun splitGraphByClock g = *)

  fun showGraph (nL, eL) =
    let 
      val nLStr = showNodeList nL
      val eLStr = showEdgeList eL
    in
      "GRAGH:\n" ^ "NODES:\n" ^ nLStr ^ "\nEDGES:\n" ^ eLStr  
    end

	fun buildGraph (dom, ran, rels) =
		let 
      val egWire1 = ref (Circuittype.WIRE (Circuittype.IN, 1, 1));
      val egEdges = exprToEdges egWire1
      val showStr = showEdge (List.hd egEdges)
      
      val egDomEdges = exprToEdges dom
      val showDomStr = showEdges egDomEdges

      val egRanEdges = exprToEdges ran
      val showRanStr = showEdges egRanEdges

      val (eL1,iL1) = appendEdgesToEdgeList egDomEdges []
      val (eL2,iL2) = appendEdgesToEdgeList egRanEdges eL1
      val g = appendRelsToGraph rels ([], eL2)
		in
      print (showStr ^ "\n");
      print (showDomStr ^ "\n");
      print (showRanStr ^ "\n");
      print ("Example Edge List: \n" ^ showEdgeList eL2 ^ "\n"); 
      print (showGraph g);
      initGraphClock 1.0 g;
      print (showGraph g)
    end
end
