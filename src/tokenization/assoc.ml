module Tokenizer : Definitions.Tokenizer.TOKENIZER with type vocabulary = (string * int) list = struct

  type vocabulary = (string * int) list

  let voc_size voc = List.length voc

  (* let add_token voc tok = 
    let max_id = voc |> List.map snd |> List.fold_left max (-1) in
    (tok, max_id+1) :: voc
 *)
  exception EncodingError of string

  let encode voc s = 
  let result = ref [] in 
  let prefixe = ref  "" in 
  let id = ref (-1) in 
  let indice_start = ref 0 in 
  let chaine = ref s in 
  let is_prefix p s = 
    let lp = String.length p in 
    let ls = String.length s in 
    let est_prefixe = ref true in 
    if lp > ls then est_prefixe := false
    else 
      for i=0 to lp-1 do 
        if p.[i] <> s.[i] then est_prefixe := false
      done;
    !est_prefixe
  in
  let rec find_longest_prefix voc s  = match voc with 
    |[] -> (!prefixe,!id)
    |(hd,i)::tl -> 
        if (is_prefix hd s) && ((String.length hd) > (String.length !prefixe)) then
          begin 
            prefixe := hd ; 
            id := i;
            find_longest_prefix tl s ;
          end 
        else find_longest_prefix tl s 
  in 
  while String.length !chaine > 0 do 
    prefixe := "" ;
    id := -1 ; 
    prefixe := fst(find_longest_prefix voc !chaine) ; 
    id := snd(find_longest_prefix voc !chaine);
    if !id= -1 then raise(EncodingError(!chaine))
    else 
      begin 
        indice_start := String.length !prefixe;
        if !indice_start <> String.length !chaine then
          begin
            chaine := String.sub !chaine !indice_start ((String.length !chaine) - !indice_start );
            result := !result@[!id];
          end 
        else 
          begin 
            result := !result@[!id];
            chaine := "";
          end
      end
  done;
  !result





  exception DecodingError of int

  let decode voc l = 
    let result = ref "" in 
    let rec find_string voc id = match voc with 
      |[] -> raise(DecodingError(id)) 
      |(hd,i)::tl -> if  i=id then result := !result^(hd) else find_string tl id 
  in 
  for i=0 to (List.length l) - 1 do 
    find_string voc (List.nth l i);
  done;
  !result 




  let learn _batch = failwith "not implemented"

end


module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer