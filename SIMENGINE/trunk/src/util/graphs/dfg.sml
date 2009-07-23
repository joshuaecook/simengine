structure DFG : DFGraph =
struct

fun new_graph () = 
    ref (Graph.empty)
    (*DirectedGraph.graph ("dfg", (), 512)*)
    

type ('a, 'b) graph = ('a, 'b) Graph.graph ref
type 'a node = Graph.node * 'a

type nodeid = int

type ('a, 'b) edge = ('a node * 'a node * 'b)

fun nodeid2label g nodeid =
    let
	val (_, _, label, _) = Graph.context (nodeid, !g)
    in
	label
    end

fun id2str id = Int.toString id

fun nodeid2node g nodeid = 
    (nodeid, nodeid2label g nodeid)
    handle e => DynException.checkpoint "DFG.nodeid2node" e

fun idofnode (nodeid, _) = nodeid

fun node2label ((_, label)) = label

val unique_nodeid = ref 0

fun gen_id () = !unique_nodeid before unique_nodeid := 1 + (!unique_nodeid)

fun add_node (graph, label) = 
    let
	val nodeid = gen_id()
	val _ = graph := Graph.embed (([], nodeid, label, []), !graph)
    in
	(nodeid,label)
    end

fun add_edge {graph, from, to, label} =
    let
	val (fromid, _) = from
	val (toid, _) = to

	val ((pred, node, nodelabel, succ), graph') = Graph.match (idofnode from, !graph)

	val _ = graph := Graph.embed ((pred, node, nodelabel, (label, idofnode to)::succ),
				      graph')

    in
	()
    end

fun delete_node (graph, node) =
    let
	val (_, graph') = Graph.match (idofnode node, !graph)
	val _ = graph := graph'
    in
	()
    end

fun nodes_equal n1 n2 =
    (idofnode n1) = (idofnode n2)
    handle e => DynException.checkpoint "DFG.nodes_equal" e

fun flatten x = foldr (op @) nil x

fun nodes graph=
    Graph.labNodes (!graph)

(* returns edge list where edge = (fromnode, tonode, label) *)
fun edges graph=
    let
	val nodeids = Graph.nodes (!graph)

	fun nodeid2outedges graph nodeid =
	    let
		val (_, _, _, succs) = Graph.context (nodeid, !graph)
	    in
		map (fn(l, n) => (nodeid2node graph nodeid, 
				  nodeid2node graph n,
				  l))
		    succs
	    end
    in
	flatten (map (nodeid2outedges graph) nodeids)
    end


fun gmap nodemap edgemap graph =
    let
	val graph' = new_graph()
	val nodes = nodes graph
	val nodes' = map (fn(n)=>add_node(graph', (nodemap n))) nodes 


	fun old2new nil node = 
	    DynException.stdException("Unmatched node ", "DFG.gmap.old2new", Logger.INTERNAL)

	  | old2new ((n,n')::rest) node 
	    = if nodes_equal n node then
		  n'
	      else
		  (old2new rest node)
	val old2new = old2new (ListPair.zip (nodes, nodes'))

	(* add edges *)
	val _ = app (fn(f,t,l)=> add_edge{graph=graph',
					  from=old2new f,
					  to=old2new t,
					  label=edgemap (f,t,l)}) (edges graph)
    in
	graph'
    end


(*
val colors = ["aquamarine",
	      (*             "black" *)
              "blue",
              "cyan",
              "darkblue",
              "darkcyan",
              "darkgreen",
              "darkgrey",
              "darkmagenta",
              "darkred",
              "darkyellow",
              "gold",
              "green",
              "khaki",
              "lightblue",
              "lightcyan",
              "lightgreen",
              "lightgrey",
              "lightmagenta",
              "lightred",
              "lightyellow",
              "lilac",
	      "magenta",
              "orange",
              "orchid",
              "pink",
              "purple",
              "red",
              "turquoise",
              "white",
	      (*             "yellow"*)
              "yellowgreen"]
*)

fun output_graph outfun graph =
    let
	fun say s = outfun s
	fun sayln s = say (s ^ "\n")

	val _ = (sayln "graph: {";
		 sayln "display_edge_labels: yes";
		 sayln "layoutalgorithm: minbackward")
	val _ = app (fn(n) =>
		       let
			   val (color, str) = node2label n
		       in
			   sayln "node: {";
			   sayln ("title: \"" ^ (Int.toString (idofnode n)) ^ "\"");
			   sayln ("label: \"" ^ str ^ "\"");
			   sayln ("color: " ^ color);
			   sayln ("}")
		       end)
		    (nodes graph)

	val _ = app (fn(f, t, s) =>
		       (sayln "edge: {";
			sayln ("sourcename: \"" ^ (Int.toString (idofnode f)) ^ "\"");
			sayln ("targetname: \"" ^ (Int.toString (idofnode t)) ^ "\"");
			sayln ("label: \"" ^ s ^ "\"");
			sayln ("}")))
		    (edges graph)

	val _ = sayln "}"
    in
	()
    end

	
(*
fun debug_visualize out graph =
    let
	fun edgelayout (_,_,edge) = 
	    [GraphLayout.LABEL (Int.toString edge)]

	fun nodelayout (_,(exp, debug)) =
	    [GraphLayout.LABEL ((exp2str exp) ^ "  " ^ debug)]

	fun graphlayout graph =
	    []

	val layout = GraphLayout.makeLayout {edge=edgelayout,
					     node=nodelayout,
					     graph=graphlayout}
					    graph
	val _ = VCG.visualize out layout
    in
	()
    end
*)


(*
fun flatten x = foldr (op @) nil x

fun pt_colorgroup_visualize out graph displayBits colorgroups =
    let
	
	fun bits2str displayBits {bits, frac, sign} =
	    if displayBits then
		"\nbits = [" ^ (Int.toString bits) 
		^ " w/ SIGN=" ^ (Int.toString sign) 
		^ ", FRAC=" ^ (Int.toString frac) ^ "]"
	    else
		""

	val bits2str = bits2str displayBits

	fun edgelayout (_,_,{argnum, bits}) = 
	    [GraphLayout.LABEL ((Int.toString argnum) (*^ " " ^ (bits2str bits)*))]
	    
	fun elem_num node (num, nil) = raise InternalError
	  | elem_num node (num, group::rest) =
	    if List.exists (fn(n)=> nodes_equal n node) group then
		num
	    else
		elem_num node (num+1, rest)


	fun nodelayout (node as (_,{bits_out, exp, lat})) =
	    let
		val bits = bits2str bits_out
		val color = if List.exists (fn(n)=>nodes_equal node n) (flatten colorgroups) then 
				List.nth (colors, elem_num node (1, colorgroups))
			    else 
				exp2color exp
	    in
		[GraphLayout.LABEL ((exp2str exp) (*^ " [LAT=" ^ (Int.toString lat) ^ ", " ^ bits ^ "]"*)),
		 GraphLayout.COLOR (color)]
	    end
	fun graphlayout graph =
	    []

	val layout = GraphLayout.makeLayout {edge=edgelayout,
					     node=nodelayout,
					     graph=graphlayout}
					    graph
	val _ = VCG.visualize out layout
    in
	()
    end
*)


fun num_nodes graph =
    Graph.noNodes (!graph)
    

fun preds (graph, (nodeid,_)) =
    let
	val (p, _, _, _) = Graph.context (nodeid, !graph)
	    handle e => DynException.checkpoint "DFG.preds.Graph.context" e
	
	fun graphedge2node (label, nodeid) = nodeid2node graph nodeid

    in
	map graphedge2node p
    end
    handle e => DynException.checkpoint "DFG.preds" e

(*fun preds (graph, node) =
    let
	val edges = edges graph
	val edges_to_node = List.filter (fn(f,t,l) => nodes_equal node t) edges
    in
	map (fn(f,t,l) => f) edges_to_node
    end
*)

fun succs (graph, (nodeid,_)) =
    let
	val (_, _, _, s) = Graph.context (nodeid, !graph)

	fun graphedge2node (label, nodeid) = nodeid2node graph nodeid
    in
	map graphedge2node s
    end

fun bottom_nodes graph =
    let
	fun is_bottom node =
	    case succs (graph, node) of
		nil => true
	      | _ => false
    in
	List.filter is_bottom (nodes graph)
    end

fun in_edges g (nodeid,_) : ('a node * 'a node * 'b) list =
    let
	val (p, _, _, _) = Graph.context (nodeid, !g)
			
	fun build_edge (label, nid) = (nodeid2node g nid, nodeid2node g nodeid, label)
    in
	map build_edge p
    end
    handle e => DynException.checkpoint "DFG.in_edges" e

fun get_edgelabel (g, from, to) =
    (case List.find (fn(f,t,l)=> nodes_equal from f andalso nodes_equal to t) 
		   (in_edges g to) of
	SOME (_,_,label) => label
      | NONE => 
	DynException.stdException("Invalid edge", "DFG.get_edgelabel", Logger.INTERNAL))
    handle e => DynException.checkpoint "DFG.get_edgelabel" e


fun delete_edge (graph, from, to) =
    let
	val (fromid, _) = from
	val (toid, _) = to

	val ((pred, node, nodelabel, succ), graph') = Graph.match (idofnode from, !graph)

	fun isntdest (l, n) =
	    toid <> n

	val succ' = List.filter isntdest succ

	open Printer

	val _ = DynException.assert (not ((length succ) <> (length succ') + 1))
				    ($("edge not correctly removed " ^ (Int.toString fromid) ^ " -> " ^ (Int.toString toid)))

	val _ = graph := Graph.embed ((pred, node, nodelabel, succ'),
				      graph')
    in
	()
    end

fun order_preds get_edgenum (g, node) =
    let
	fun insert (p,l) nil =
	    [(p,l)]
	  | insert (p,l) ((p',l')::rest) =
	    (if l < l' then
		 (p,l)::(p',l')::rest
	     else
		 (p',l')::(insert (p,l) rest))
	    handle e => DynException.checkpoint "DFG.order_preds.insert" e

	fun isort alist nil = alist
	  | isort alist ((p,l)::rest) =
	    isort (insert (p,l) alist) rest

	val preds = preds (g, node)
	val labels = map (fn(p)=> get_edgenum (g, p, node)) preds
	val sorted = isort nil (ListPair.zip (preds, labels))
	val (opreds, indices) = ListPair.unzip sorted
    in
	opreds
    end
    handle e => DynException.checkpoint "DFG.order_preds" e


fun reachable graph (nodes)  =
    let
	fun reach node =
	    node :: (flatten (map reach (succs (graph, node))))

	val reaches = flatten (map reach (flatten (map (fn(n)=> succs (graph, n)) nodes)))
	
	fun isin node nil = false
	  | isin node (node'::rest) =
	    (nodes_equal node node') orelse (isin node rest)

	fun uniquify nil = nil
	  | uniquify (node::rest) = 
	    if isin node rest then
		uniquify rest
	    else
		node :: (uniquify rest)
    in
	uniquify reaches
    end

(* finds all nodes that, through some path, are a predecessor of node in graph *)
fun pred_tree graph node =
    let
	fun reach node =
	    node :: (flatten (map reach (preds (graph, node))))
	    
(*	val reaches = flatten (map reach (flatten (map (fn(n)=> preds (graph, n)) (nodes graph)))) *)
	val reaches = flatten (map reach (preds (graph, node)))
	
	fun isin node nil = false
	  | isin node (node'::rest) =
	    (nodes_equal node node') orelse (isin node rest)

	fun uniquify nil = nil
	  | uniquify (node::rest) = 
	    if isin node rest then
		uniquify rest
	    else
		node :: (uniquify rest)
    in
	uniquify reaches
    end


fun union (g, g') =
    let
	val g'' = ref (!g')

	val old2newnodes = map (fn(n)=> (n, (add_node(g'', node2label n)))) (nodes g)

	fun old2new node =
		case List.find (fn(old,new) => nodes_equal old node)
			       old2newnodes of
		    SOME (old, new) => new
		  | NONE => 
		    DynException.stdException("Unmatched node ", "DFG.union.old2new", Logger.INTERNAL)

	val _ = app (fn(e) => 
		       let 
			   val (from, to, label) = e
		       in
			   add_edge {graph=g'',
				     from=old2new from,
				     to=old2new to,
				     label=label}
		       end)
		    (edges g)

    in
	g''
    end

fun copy g =
    let
	val g' = union (g, new_graph())
		 
	open Printer

	val _ = DynException.assert ((num_nodes g) = (num_nodes g'))
				    ($("Copy failed with unequal number of nodes in old and new graph.  Old: " ^ (Int.toString (num_nodes g)) ^ " New: " ^ (Int.toString (num_nodes g'))))
    in
	g'
    end

fun build_graph nodes edges =
    let
	fun add_node ((nodeid, label), graph) = 
	    let
		val _ = graph := Graph.embed (([], nodeid, label, []), !graph)
	    in
		graph
	    end

	fun add_edge ((fromid, toid, label), graph) =
	    let
		val ((pred, node, nodelabel, succ), graph') = Graph.match (fromid, !graph)

		val _ = graph := Graph.embed ((pred, node, nodelabel, (label, toid)::succ),
					      graph')
	    in
		graph
	    end

	val g = new_graph ()
	val g = foldl add_node g nodes
	val g = foldl add_edge g edges
    in
	g
    end

fun replace_nodelabel (graph, (nodeid, _), newlabel) =
    let
	val ((pred, node, nodelabel, succ), graph') = Graph.match (nodeid, !graph)

	val _ = graph := Graph.embed ((pred, node, newlabel, succ),
				      graph')
    in
	graph
    end


open mlJS

fun node2json (graph, conv_func) node =
    let
	val label_json = conv_func (node2label node)
	val args = preds (graph, node)
    in
	js_object [("node", js_object [("id", js_int (idofnode node)),
				       ("label", label_json),
				       ("args", js_array (map (node2json (graph, conv_func)) args))])]
    end

fun roots graph = 
    List.mapPartial 
	(fn(n)=> if List.length (succs (graph, n)) = 0 then
		     SOME n
		 else
		     NONE)
	(nodes graph)


fun graph2json (graph, conv_func) =
    js_object [("graph", js_array 
			    (map
				 (node2json (graph, conv_func))
				 (roots graph)))]
	
end
