
open Definitions.MarkovChain
let learn_markov_chain ~token_of_arc ~max_state_id ~walks =
  (* walks = liste de marches, i.e. de suites aléatoires d'états*)
  let mc = Array.init max_state_id (fun _ -> []) in 
  let count walk = 
    match walk with 
    |[] | [_] -> ()
    |_::tl ->
      List.iter2
        (fun src dst ->
          let token = token_of_arc src dst in 
          let edge = { token ; weight = 0; dest = dst} in 
          mc.(src) <- edge :: mc.(src))
        walk tl
  in 
  List.iter count walks;
  let f edges = 
    let poids_total = List.fold_left (fun acc {weight; _} -> acc + weight) 0 edges in 
    List.map (fun edge -> {edge with weight = (edge.weight*100)/ poids_total }) edges
  in 
  let m = Array.map f mc in 
  MarkovChain m