signature DFGraph = 
sig

(* 'a is the type of a node label; 'b is the type of an edge label. *)
type ('a, 'b) graph
type 'a node
type nodeid = int

type ('a, 'b) edge = ('a node * 'a node * 'b)
    
val num_nodes : ('a, 'b) graph -> int 
val node2label : 'a node -> 'a
val add_node : ('a, 'b) graph * 'a -> 'a node
val add_edge : {graph: ('a , 'b) graph, from: 'a node, to: 'a node, label: 'b} -> unit
val delete_node : ('a , 'b) graph * 'a node-> unit
val delete_edge : ('a, 'b) graph * 'a node * 'a node -> unit
val new_graph : unit -> ('a , 'b) graph
val copy : ('a, 'b) graph -> ('a, 'b) graph
val get_edgelabel : ('a, 'b) graph * 'a node * 'a node -> 'b
val nodes : ('a, 'b) graph -> 'a node list
val bottom_nodes : ('a, 'b) graph -> 'a node list
val edges : ('a, 'b) graph -> ('a, 'b) edge list
val succs : ('a, 'b) graph * 'a node -> 'a node list
val preds : ('a, 'b) graph * 'a node -> 'a node list
val roots : ('a, 'b) graph -> 'a node list
val gmap : ('a node -> 'c) -> 
	   ('a node * 'a node * 'b -> 'd) -> 
	   ('a, 'b) graph -> 
	   ('c, 'd) graph
val nodes_equal : 'a node -> 'b node -> bool
val idofnode : 'a node -> nodeid
val order_preds: ((('a, 'b) graph * 'a node * 'a node) -> int) -> ('a, 'b) graph * 'a node -> 'a node list
			            (*(color * nlabel, elabel) *)
val output_graph : (string -> unit) -> (string * string, string) graph -> unit

val in_edges : ('a, 'b) graph -> 'a node -> ('a, 'b) edge list
val union : ('a, 'b) graph * ('a, 'b) graph -> ('a, 'b) graph
val reachable : ('a, 'b) graph -> 'a node list -> 'a node list
val pred_tree : ('a, 'b) graph -> 'a node -> 'a node list
(* build_graph should only be used by a graph input/output function.
   Don't use if you don't know what you are doing. *)
val build_graph: (nodeid * 'a) list -> (nodeid * nodeid * 'b) list -> ('a, 'b) graph
val replace_nodelabel : ('a, 'b) graph * 'a node * 'a -> ('a, 'b) graph

val node2json : ('a, 'b) graph * ('a -> mlJS.json_value) -> 'a node -> mlJS.json_value
val graph2json : ('a, 'b) graph * ('a -> mlJS.json_value) -> mlJS.json_value

end
