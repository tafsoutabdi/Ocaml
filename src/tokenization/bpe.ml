open Definitions.Tokenizer


module Make (InitialTokenizer:TOKENIZER) = struct

  let max_vocab_size = ref 1000

  type merge_rule = ((int * int) * int)
  (* exemple 
     supposons que 
        - dans le vocabulaire initial l'id de "l" est 13   
        - dans le vocabulaire initial l'id de "e" est 4   
        - on veut représenter la règle "l+e -> le" 
        - on a choisi l'id 4 pour le nouveau token "le"
      alors on utilise `((13, 4), 7)`
    *)

  type vocabulary = {
    initial_vocabulary: InitialTokenizer.vocabulary;
    merge_rules: merge_rule list
  }

  let voc_size voc = 
    InitialTokenizer.voc_size voc.initial_vocabulary +
    List.length voc.merge_rules

  exception EncodingError of string

  let encode voc s =
    ignore (voc, s);
    failwith "todo"  

  exception DecodingError of int

  let decode voc ids =
    ignore (voc, ids);
    failwith "todo"  

    let learn(initial_vocabulary: string list) batch = 
    let calcul_frequent_pairs vocab = 
      let pairs_frequencies = Hashtbl.create 100 in 
      let cal_pair_freq pair = 
        let c = try Hashtbl.find pairs_frequencies pair with Not_found -> 0 in 
        Hashtbl.replace pairs_frequencies pair (c+1)
      in 
     
      let process_s symb = 
        let rec consecutive_pairs lst acc = 
          match lst with 
          |x::y::tl -> consecutive_pairs (y::tl) ((x,y)::acc)
          |_-> List.rev acc
        in
       
        let pairs = List.map (fun sym -> String.to_seq sym |> List.of_seq |> consecutive_pairs []) symb in 
        List.iter (fun p -> List.iter cal_pair_freq p) pairs
      in 
      let process_voc vocabulary = 
        Hashtbl.iter (fun _ symbole -> process_s symbole) vocabulary
      in 
      process_voc vocab;
      pairs_frequencies
    in
    let rec merge_most_frq_p vocab = 
        let pairs_frequencies = calcul_frequent_pairs vocab in 
        let most_frq_p = 
          Hashtbl.fold
            (fun pair fr ((max_p1,max_p2),max_f)->
              if fr > max_f then (pair,fr) else ((max_p1,max_p2),max_f))
            pairs_frequencies
            (("",""),0)
        in 
        let mrg_vocabulary = 
          merge_pair_into_vocab vocab (fst most_frq_p) in
        in 
        most_frq_p, mrg_vocabulary
    in

    let rec mrg_frq_pairs vocab = 
        let _,merged_vocabulary = merge_most_frq_p vocab in
        let new_voc_size = voc_size mrg_vocabulary in 
        if new_voc_size <= !max_vocab_size then
          mrg_vocabulary
        else 
          mrg_frq_pairs mrg_vocabulary
        in 
    let initial_vocabulary = 
        build_initial_vocabulary initial_vocabulary
      in
    mrg_frq_pairs initial_vocabulary

end

module MakeCheckType : functor (InitialTokenizer:TOKENIZER) -> TOKENIZER = Make