exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph = {
  mutable g_nodes : 'a node list
}
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

(* Creer un graphe vide *)
let mk_graph () = { g_nodes = [] }

(* Ajouter un noeud *)
let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

(* Ajouter une arete  *)
let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

(* Detection de cycles *)
let has_cycle g = failwith "Non implemente"

(* Renvoie les noeuds dans un ordre topologique *)
let topological g = failwith "Non implemente"

