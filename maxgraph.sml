
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
*
* This code works like this: 
**)

structure MaxGraph :
  sig

  	type edge
    type node
    type graph
    type kernel 
    type manager

 		val splitKernel : Circuittype.pcircuit -> (kernel list) * manager
  end =
struct 

  type kernel   = (string * Circuittype.pcircuit)
  type manager  = (kernel * real) list

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

  fun eqClk (clk1 : real) (clk2 : real) = 
    let 
      val clkInt1 = Real.toInt IEEEReal.TO_NEAREST clk1
      val clkInt2 = Real.toInt IEEEReal.TO_NEAREST clk2
    in 
      clkInt1 = clkInt2
    end

  fun isEdgeOutput (Edge (SOME(_),NONE,_,_)) = true
    | isEdgeOutput _                         = false 

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
        NONE      => (Errors.simple_error ("ERROR: findEdgeById - Can't find edge " ^ Int.toString id))
      | SOME(x,e) => e)
    end


  (** Getters for edge **)
  fun getEdgeId (Edge (_,_,x,_)) = x

  fun getEdgeToNodeId (Edge (_,t,_,_)) = t
    | getEdgeToNodeId _ = (Errors.simple_error "ERROR: getEdgeToNodeId - Not common Edge type")

  fun getEdgeFromNodeId (Edge (t,_,_,_)) = t
    | getEdgeFromNodeId _ = (Errors.simple_error "ERROR: getEdgeFromNodeId - Not common Edge type")

  fun getEdgeClock (Edge (_,_,_,SOME(clk))) = clk
    | getEdgeClock _ = (Errors.simple_error "ERROR: getEdgeClock - Not common Edge type")

  fun getEdgeDir (Edge (SOME(_),NONE,_,_)) = Circuittype.OUT
    | getEdgeDir (Edge (NONE,SOME(_),_,_)) = Circuittype.IN
    | getEdgeDir _                         = (Errors.simple_error "ERROR: Can't get direction for this edge type")

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
        NONE      => (Errors.simple_error ("ERROR: findNodeById - Can't find node " ^ Int.toString nodeId))
      | SOME(x,n) => n )
    end
  (** another copy paste ... **)
  fun getEdgePrevNode e (nL: (int * node ref) list) = 
    case (getEdgeFromNodeId e) of
      NONE    => NONE
    | SOME(t) => SOME (findNodeById t nL)

  fun getEdgeNextNode e nL = 
    case (getEdgeToNodeId e) of
      NONE    => NONE
    | SOME(t) => SOME (findNodeById t nL)

  fun getNodeInEdges  (Node (es,_,_)) = es

  fun getNodeOutEdges (Node (_,es,_)) = es

  fun getNodeDevice (Node (_,_,d)) = d

  (** 
   * This is an ad-hoc solution: we just check the device type of node,
   * if it's pdsr, we times n;
   * if it's sdpr, we div n;
   * else, just remain the original value.
   **)
  fun getNodeClockScale (Node (_,_,Circuittype.PDSR n)) = Real.fromInt n
    | getNodeClockScale (Node (_,_,Circuittype.SDPR n)) = 1.0 / (Real.fromInt n)
    | getNodeClockScale _ = 1.0

  (** quite copy-paste, how to optimize this? **)
  fun resetNodeInputList (Node (iL,oL,d)) i0 i1 =
    let 
      val pop = List.filter (fn (x) => not (x = i0)) iL
      val ins = i1::pop
    in 
      Node (ins, oL, d)
    end

  fun resetNodeOutputList (Node (iL,oL,d)) i0 i1 =
    let 
      val it = print ("Remove: " ^ Int.toString i0 ^ " to: " ^ Int.toString i1 ^ "\n")
      val pop = List.filter (fn (x) => not (x = i0)) oL
      val ins = i1::pop
    in 
      Node (iL, ins, d)
    end

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

  fun isIdInGraphList x []         = false
    | isIdInGraphList x ((y,_)::l) =
    ( if (y = x) then true else (isIdInGraphList x l) )

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
        (Edge (NONE,SOME(_),_,_)) => (getGraphInputEdges (nL,es))@[x]
      | _                         => (getGraphInputEdges (nL,es)) )

  fun getGraphOutputEdges (_,[])            = []
    | getGraphOutputEdges (nL,((x,e)::es))  = 
    ( case !e of
        (Edge (SOME(_),NONE,_,_)) => (getGraphOutputEdges (nL,es))@[x]
      | _                         => (getGraphOutputEdges (nL,es)) )

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

  fun getMaxIdinGraphList []         n = n
    | getMaxIdinGraphList ((x,_)::l) n =
      getMaxIdinGraphList l (if (n > x) then n else x)

  (** 
   * [splitEdgeInGraph]: split a specific edge by id.
   * This edge should be an edge which connects a high speed node and a low speed 
   * node. The speed of node depends on the **input** edges.
   * TRICKY!!! After this function, the node list has been changed as each node is 
   * a reference. 
   * Example:
   * If there's an edge with id [x] will be splitted, and this edge has content:
   * Edge (f,t,id,clk). First we will build 2 new Edges, with NONE input or NONE output
   * Filter out the origin edge id from connected nodes, and insert these 2 new ones.
   **)

  fun splitEdgeInGraph x ((nL, eL):graph) =
    let 
      val e = findEdgeById x eL
    in 
    ( case !e of
        Edge (f, t, id, clk) => 
        ( let 
            val e1 = Edge (f, NONE, id, clk)
            val e2 = Edge (NONE, t, id, clk)
            val i1 = (getMaxIdinGraphList eL 0) + 1
            val eL1= eL @[(i1,ref e1)]
            val i2 = (getMaxIdinGraphList eL1 0) + 1
            val eL2= eL1@[(i2,ref e2)]
            val prev = getEdgePrevNode (!e) nL
            val next = getEdgeNextNode (!e) nL  
          in
          ( (print ("Edge id: " ^ Int.toString x ^ 
                    " new output: " ^ Int.toString i1 ^ 
                    " new input: " ^ Int.toString i2 ^ "\n"));
            (case prev of SOME(n) => (n := resetNodeOutputList (!n) x i1) | _ => ());
            (case next of SOME(n) => (n := resetNodeInputList  (!n) x i2) | _ => ());
            (e := Edge (NONE, NONE, id, clk));
            (nL, eL2) )
          end )
      | _ => (nL, eL) (* will do nothing for this case *) )
    end

  fun filterEdgeInEdgeList []          = []
    | filterEdgeInEdgeList ((x,e)::es) =
    ( case !e of
        Edge (NONE,NONE,_,_) => filterEdgeInEdgeList es
      | _                    => (x,e)::(filterEdgeInEdgeList es) )

  (**
   * splitAndFilterEdgesInGraph:
   * Give a list of edge ids, which will be splitted. Split each of them and get the final graph.
   * And at last, filter out all those edges without input and output nodes (NONE,NONE).
   **)
  fun splitAndFilterEdgesInGraph es (nL,eL) =
    let 
      (* new graph *)
      val (nL1,eL1) = List.foldl (fn(x,g)=>splitEdgeInGraph x g) (nL,eL) es
      (* filtered graph *)
      val eL2       = filterEdgeInEdgeList eL1
    in 
      (nL1, eL2) 
    end


  (* from a list of input nodes, generate a **set** of node ids *)
  fun getNodeIdsFromEdgeIds []      (nL,eL) = []
    | getNodeIdsFromEdgeIds (x::xs) (nL,eL) =
    ( let 
        val e  = findEdgeById x eL
        val ni = getEdgeToNodeId (!e)
        val ns = getNodeIdsFromEdgeIds xs (nL,eL)
      in 
      ( case ni of
          NONE    => ns
        | SOME(i) => if (List.exists (fn (y)=> i=y) ns) then ns else i::ns )
      end )

  fun getEdgeIdsFromNodeIds []      g       = []
    | getEdgeIdsFromNodeIds (x::xs) (nL,eL) = 
    ( let 
        val n   = findNodeById x nL
        val es  = getNodeOutEdges (!n)
        val debug = (print ("getEdgeIdsFromNodeIds: " ^ Int.toString x ^ " -> [" ^ (Utilities.concatWith "," (List.map Int.toString es)) ^ "]\n"))
      in 
        es@(getEdgeIdsFromNodeIds xs (nL,eL))
      end )

  fun getNodesFromNodeIds []      g       = []
    | getNodesFromNodeIds (x::xs) (nL,eL) = 
      (findNodeById x nL)::(getNodesFromNodeIds xs (nL,eL))

  (**
   * isDiffSpeedNode: for a node with id x, ditect whether its input and output edges
   * has different clock rate. WE ASSUME that input and output edges for a simple node
   * should have same clock rate. (So we use List.hd here)
   **)
  fun isDiffSpeedNode x ((nL,eL) : graph) =
    let 
      val n  = findNodeById x nL
      val ie = findEdgeById (List.hd (getNodeInEdges  (!n))) eL
      val oe = findEdgeById (List.hd (getNodeOutEdges (!n))) eL
      val c1 = getEdgeClock (!ie)
      val c2 = getEdgeClock (!oe)
    in
      not (eqClk c1 c2)
    end

  (* A helper function to get the compliment of 2 lists O(N*M) solution ewww... *)
  fun getListCompliment []      ys = ys 
    | getListCompliment (x::xs) ys = 
    ( let val nys = (List.filter (fn(y)=>not (x=y)) ys) in
        getListCompliment xs nys
      end )

  (**
   * iterFindSplitEdges:
   * give a list of input edges, generate next to search edge ids and a list
   * of to-split edges. NOTICE THAT this function only return the **first livel** of 
   * should-split edges.
   * If we found one edge that should be splitted, we ignore it in the next iteration.
   *)
  fun iterFindSplitEdges [] g = []
    | iterFindSplitEdges es g = 
    ( let 
        (* Next level nodes *)
        val next = getNodeIdsFromEdgeIds es g 
        (* Should split nodes in next level *)
        val diff = List.filter (fn (x) => (isDiffSpeedNode x g)) next 
        (* Get the next level nodes' output edges *)
        val nes  = getEdgeIdsFromNodeIds next g
        (* Get the next level should-split nodes' output edges *)
        val ses  = getEdgeIdsFromNodeIds diff g
        (* Get the compliment set of nes\ses *)
        val ret  = iterFindSplitEdges (getListCompliment ses nes) g
      in 
        ses@ret
      end )

  (** 
   * iterBuildGraph:
   * Give a list of input edges, iterate all the connected node and edge in the graph. Put them 
   * in the result graph.
   * return a new graph.
   * Please notice that this input graph (nL,eL) is a splitted one, or the output graph will be 
   * the same as input graph.
   * And the original input edges should only contain the original input, not the splitted input.
   **)
  fun iterBuildGraph []      (nL,eL) (nnL,neL) = (nnL,neL) (* if empty input, then return new graph *)
    | iterBuildGraph (x::xs) (nL,eL) (nnL,neL) =
    ( let 
        val e   = findEdgeById x eL
        val eL1 = if (isIdInGraphList x neL) then neL else (x,e)::neL (* If not in list, append it *)
      in
      ( case (getEdgeToNodeId (!e)) of
          NONE    => iterBuildGraph xs (nL,eL) (nnL,eL1) (* Just return *)
        | SOME(ni)=> 
        ( let 
            val n = findNodeById ni nL 
            val nL1        = if (isIdInGraphList ni nnL) then nnL else (ni, n)::nnL 
            val (nL2, eL2) = iterBuildGraph xs (nL,eL) (nL1,eL1)
            val nxs        = getNodeOutEdges (!n)
          in 
            (* we should go to the next level *)
            iterBuildGraph nxs (nL,eL) (nL2,eL2)
          end ))
      end )

  fun filterExistEdgeList []      (nL,eL) = []
    | filterExistEdgeList (x::xs) (nL,eL) =
    ( if (isIdInGraphList x eL) 
      then x::(filterExistEdgeList xs (nL,eL))
      else (filterExistEdgeList xs (nL,eL)) )
  
  fun showGraph (nL, eL) =
    let 
      val nLStr = showNodeList nL
      val eLStr = showEdgeList eL
    in
      "GRAGH:\n" ^ "NODES:\n" ^ nLStr ^ "\nEDGES:\n" ^ eLStr  
    end

  (**
   * [iterSplitGraph]: 
   * Take a list of input edges, and a history edge id list, which is those input edges that
   * has been visited. 
   * This function will generate a graph from current input es, and get a list of graphs in the next 
   * steps. The next step's input edges are those: not in es, but input edges. The history list should
   * also be updated.
   **)
  fun iterSplitGraph [] hs g = []
    | iterSplitGraph es hs g =
    ( let 
        val splitEdgeIds = iterFindSplitEdges es g
        val debug = 
        ( (print ("INPUT ids:  \t[" ^ (Utilities.concatWith ", " (List.map Int.toString es)) ^ "]\n"));
          (print ("HISTORY ids:\t[" ^ (Utilities.concatWith ", " (List.map Int.toString hs)) ^ "]\n"));
          (print ("SPLIT ids:  \t[" ^ (Utilities.concatWith ", " (List.map Int.toString splitEdgeIds)) ^ "]\n")))
        val splitNewGraph= splitAndFilterEdgesInGraph splitEdgeIds g
        val newInEdgeIds = getGraphInputEdges splitNewGraph
        val nextHistory  = es @ hs
        (* only those edges which not in the previous input set and currently
         * in the input edges of splitted new graph will be added *)
        val nextInEdgeIds= filterExistEdgeList (getListCompliment nextHistory newInEdgeIds) splitNewGraph
        val build        = iterBuildGraph es splitNewGraph ([],[])
        val it           = print (showGraph splitNewGraph)
        val retGraphList = iterSplitGraph nextInEdgeIds nextHistory splitNewGraph
      in 
        build::retGraphList
      end )


  fun splitGraphToGraphs (g:graph) = 
    let
      val inEdgeIds = getGraphInputEdges g
    in
      (iterSplitGraph inEdgeIds [] g)
    end

  (**
   * Convert back to ruby expressions
   
   **)

  fun fromEdgeIdsToListOfExpr []      (nL,eL) dirOption = []
    | fromEdgeIdsToListOfExpr (x::xs) (nL,eL) dirOption =
    ( let
        val e   = findEdgeById x eL
        val id  = getEdgeId  (!e)
        val dir = ( case dirOption of SOME(d) => d | NONE => getEdgeDir (!e) )
        val expr= ref (Circuittype.WIRE (dir,id,1)) (* the out value is useless for printmax *) 
      in 
        expr::(fromEdgeIdsToListOfExpr xs (nL,eL) dirOption)
      end )

  fun fromEdgeIdsToWrapExpr es g dirOption = 
    let 
      val exprs = fromEdgeIdsToListOfExpr es g dirOption
      val expr  = ( if ((List.length exprs) = 1) then (List.hd exprs) else ref (Circuittype.LIST exprs) )
    in 
      expr
    end 

  fun fromNodeToGate nId (nL,eL) =
    let
      val n          = findNodeById nId nL
      val inEdgeIds  = getNodeInEdges  (!n)
      val outEdgeIds = getNodeOutEdges (!n)
      val inExpr     = fromEdgeIdsToWrapExpr inEdgeIds  (nL,eL) (SOME Circuittype.IN)
      val outExpr    = fromEdgeIdsToWrapExpr outEdgeIds (nL,eL) (SOME Circuittype.OUT)
      val device     = getNodeDevice (!n)
    in 
      (device, inExpr, outExpr)
    end

  fun filterOutputEdges []      g       = []
    | filterOutputEdges (x::xs) (nL,eL) = 
    ( let 
        val e = findEdgeById x eL
        val r = filterOutputEdges xs (nL,eL)
        val debug = 
        ( print ((Int.toString x) ^ (if (isEdgeOutput (!e)) then " OUTPUT " else " NOT OUTPUT ") ^ "\n"); 
          print (showGraph (nL,eL)) )
      in 
      ( if isEdgeOutput (!e) then r else x::r )
      end )

  fun fromEdgeIdsToRelationAndNextEdgeIds es (nL,eL) = 
    let
      val d1      = print "fromEdgeIdsToRelationAndNextEdgeIds START\n"
      val ns      = getNodeIdsFromEdgeIds es (nL,eL)
      val relation= List.foldl (fn(x,l)=>(fromNodeToGate x (nL,eL))::l) [] ns
      val nIds    = getEdgeIdsFromNodeIds ns (nL,eL)
      val nextIds = filterOutputEdges nIds (nL, eL)
      val debug   = 
      ( print ("Node ids:     \t[" ^ (Utilities.concatWith ", " (List.map Int.toString ns)) ^ "]\n");
        print ("From Edge ids:\t[" ^ (Utilities.concatWith ", " (List.map Int.toString es)) ^ "]\n");
        print ("Next Edge ids:\t[" ^ (Utilities.concatWith ", " (List.map Int.toString nIds)) ^ "]\n");
        print ("Filter ids:   \t[" ^ (Utilities.concatWith ", " (List.map Int.toString nextIds)) ^ "]\n") )
    in
      (relation,nextIds)
    end

  fun iterBuildRelations [] g = []
    | iterBuildRelations es g =
    ( let 
        val (rel,nes) = fromEdgeIdsToRelationAndNextEdgeIds es g
        val debug     = (print ("Edge ids:  \t[" ^ (Utilities.concatWith ", " (List.map Int.toString es)) ^ "]\n"))
        val rels      = iterBuildRelations nes g
      in
        rel::rels
      end ) 

  fun graphToPCircuit g = 
    let 
      val inEdgeIds  = getGraphInputEdges  g
      val outEdgeIds = getGraphOutputEdges g
      val inExpr     = fromEdgeIdsToWrapExpr inEdgeIds  g NONE
      val outExpr    = fromEdgeIdsToWrapExpr outEdgeIds g NONE
      val rels       = iterBuildRelations inEdgeIds g
    in
      (inExpr,outExpr,rels)
    end

  fun graphsToPCircuits []      = []
    | graphsToPCircuits (g::gs) = 
    ( let 
        val pc  = graphToPCircuit   g
        val pcs = graphsToPCircuits gs 
      in 
        pc::pcs
      end )

  fun genKernelNameList 0 = []
    | genKernelNameList n = 
      (genKernelNameList (n-1))@[("Kernel"^Int.toString n)]

  fun genClockList []             = []
    | genClockList ((nL,eL)::gs)  = 
    ( let 
        val es  = getGraphInputEdges (nL,eL) 
        val clk = getEdgeClock (!(findEdgeById (List.hd es) eL))
      in 
        clk::(genClockList gs)
      end )

	fun splitKernel (dom, ran, rels) =
		let 
      val (eL1,iL1) = appendEdgesToEdgeList (exprToEdges dom) []
      val (eL2,iL2) = appendEdgesToEdgeList (exprToEdges ran) eL1
      val g         = appendRelsToGraph rels ([], eL2)
      val setClk    = (initGraphClock 1.0 g; print (showGraph g))
      val splitGs   = splitGraphToGraphs g
      val pcs       = graphsToPCircuits splitGs
      val clks      = genClockList splitGs
      val kernels   = ListPair.zip ((genKernelNameList (List.length pcs)),pcs)
      val manager   = ListPair.zip (kernels, clks)
		in
      print "SPLIT GRAPHS:\n";
      List.app (fn (x) => print (showGraph x)) splitGs;
      (kernels, manager)
    end
end
