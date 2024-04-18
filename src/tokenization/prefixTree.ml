module Tokenizer : Definitions.Tokenizer.TOKENIZER = struct

  type prefix_tree = Node of node

  and node = {
    mutable id: int option;
    mutable successors: (char * prefix_tree) list
  }

  type vocabulary = {
    prefix_tree: prefix_tree;
    token_of_id: string option array
  }

  let voc_size voc = 
    voc.token_of_id 
    |> Array.map (function None -> 0 | Some _ -> 1)
    |> Array.fold_left (+) 0

  exception EncodingError of string

  let encode_aux prefix_tree s =
  let result = ref [] in 
  let num = ref (-1) in 
  let p = ref "" in 
  let chaine = ref s in 
  let find_prefix prefix_tree s = 
    match prefix_tree with 
    |Node n -> 
        let rec find_char_in_successors c l = 
          match l with 
          |[] -> []
          |(ch,tree)::_ when ch=c -> 
              let get_node tree = 
                match tree with 
                |Node n -> n 
              in [(ch , get_node tree)]
          |_::tl -> find_char_in_successors c tl
        in 
        let prefixe = ref "" in 
        let prefixe_en_cours = ref "" in 
        let noeud = ref n in 
        let chr = ref ' ' in 
        let i = ref 0 in 
        let identifiant = ref (-1) in
        let int_opt_to_int num = 
          match num with 
          |None -> !identifiant
          |Some x -> x
        in
        while  !i < String.length s && (find_char_in_successors s.[!i] !noeud.successors ) <> [] do 
          chr := fst (List.nth (find_char_in_successors s.[!i] !noeud.successors) 0) ;
          noeud := snd (List.nth (find_char_in_successors s.[!i] !noeud.successors) 0) ;
          prefixe_en_cours := !prefixe_en_cours ^ (String.make 1 !chr) ;
          if !noeud.id <> None then 
            begin 
              identifiant := int_opt_to_int !noeud.id ;
              prefixe := !prefixe ^ !prefixe_en_cours ;
              prefixe_en_cours := "" ; 
            end;
          i := !i + 1 ; 
        done; 
        (!identifiant, !prefixe) 
  in
  while String.length !chaine > 0 do 
    num := fst (find_prefix prefix_tree !chaine) ;
    p := snd (find_prefix prefix_tree !chaine) ;
    if !num = (-1) then raise (EncodingError !chaine) 
    else 
      begin 
        result := !result @ [!num] ; 
        if (String.length !chaine) - (String.length !p) = 0 then chaine := "" 
        else chaine := String.sub !chaine (String.length !p) ( (String.length !chaine) - (String.length !p) ) ;
      end ;
  done;
  !result

  let encode voc s = encode_aux voc.prefix_tree s

  exception DecodingError of int

  let decode voc l = 
    let result = ref "" in 
    let id = ref 0 in 
    let get_string s = 
      match s with 
      |None -> "" 
      |Some x -> x 
    in
    for i=0 to (List.length l) - 1  do 
      id := List.nth l i ;
      if !id < 0 || !id >= Array.length voc.token_of_id then raise (DecodingError !id) 
      else 
        begin 
          if voc.token_of_id.(!id) <> None then 
            result := !result ^ ( get_string (voc.token_of_id.(!id)) ) 
          else raise (DecodingError !id);
        end;
    done;
    !result 

  let vocabulary_of_assoc_list l =
    ignore l;
    failwith "todo!"

  let learn _batch = failwith "not implemented" 

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer 