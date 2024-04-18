module Tokenizer : Definitions.Tokenizer.TOKENIZER with type vocabulary = char list = struct

  type vocabulary = char list

  let voc_size voc = List.length voc

  exception EncodingError of string

  let encode voc s =
    let result = ref [] in 
    let len = voc_size voc in 
    let lg = String.length s in 
    let found = ref false in
    for i=0 to lg - 1 do 
      found := false;
      for j=0 to len - 1 do 
        if s.[i] = List.nth voc j then 
          begin 
            found := true;
            result := !result@[j] ; 
          end
      done;
      if !found = false then raise( EncodingError( String.sub s i (lg - i) ) )
  done;
  !result

  exception DecodingError of int

  let decode voc ids =
    let result = ref "" in 
    let len = voc_size voc in 
    let lg = List.length ids in 
    for i=0 to lg - 1 do
      let index = List.nth ids i in
      if index > len || index < 0 then raise( DecodingError (index) )
      else result := !result^(String.make 1 (List.nth voc index ) )
    done;
    !result
 
  let learn batch = 
  let result : vocabulary ref = ref [] in 
  let rec presence voc car = match voc with
    |[] -> false
    |hd::tl -> hd=car || presence tl car 
  in
  let l = List.length batch in
  for i = 0 to l-1 do
    let s = String.length (List.nth batch i) in
    for j = 0 to s-1 do 
      if not (presence !result (String.get (List.nth batch i) j)) then
        result:=!result @ [String.get (List.nth batch i) j]
    done;
  done;
  !result

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer