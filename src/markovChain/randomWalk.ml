open Definitions.MarkovChain
let random_walk ~length ?(start = 0) (mc: 'a Definitions.MarkovChain.markov_chain) : 'a list =
  (* renvoie la suite de token étiquettant une marche aléatoire de longueur
     `length` au plus; si la marche aléatoire atteint un état
     sans successeurs, on s'arrête et on renvoie la marche obtenue
  *)
   let next_token current_state = 
      let edges = match mc with MarkovChain arr -> arr.(current_state) in 
      let poids_total = List.fold_left(fun acc edge -> acc + edge.weight) 0 edges in
      if poids_total <= 0 then
         failwith "poids total nul ou negatif"
      else
         let c = Random.int poids_total in
         let rec selectionner acc_poids = function
            |[] -> failwith "transition impossible"
            |edge :: tl -> 
               if acc_poids + edge.weight > c then [edge.token]
               else selectionner (acc_poids + edge.weight)  tl
         in
         selectionner 0 edges
   in 
   let rec walk pas current_state current_walk = 
      if pas = length then current_walk
      else 
         let token = next_token current_state in 
         let next_edges = match mc with MarkovChain arr -> arr.(current_state)in 
         match next_edges with 
         |[] -> current_walk
         |hd :: _ -> 
            let next_state = hd.dest in 
            walk (pas+1) next_state (current_walk@ token)
   in 
   walk 0 start []