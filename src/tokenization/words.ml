module Tokenizer : Definitions.Tokenizer.TOKENIZER with type vocabulary = (string * int) list = struct

  type vocabulary = (string * int) list

  let voc_size voc = List.length voc

  exception EncodingError of string


  let is_alpha = function 
    | 'a' .. 'z' | 'A' .. 'Z'  -> true 
    | c -> begin
        let accented_characters = 
          "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæ" ^ 
          "çèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČč" ^
          "ĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴ" ^
          "ĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚś" ^
          "ŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽž"
        in String.contains accented_characters c
      end


  let first_non_alpha_from s start =
      (* renvoie l'indice du premier caractère non alphabetique 
         de la chaine s en partant de l'indice start
         si ce caractère n'existe pas, renvoie la longueur de s 
         exemples:
          - `first_non_alpha_from "hello world" 3` renvoie 5
          - `first_non_alpha_from "hello world" 5` renvoie 5
          - `first_non_alpha_from "hello world" 6` renvoie 11       
    *)
    let lg = String.length s in
    let res = ref lg in
    for i = start to lg - 1 do
      if not(is_alpha s.[i]) && !res=lg then res:= i  
    done;
    !res

  let alpha_blocks s = 
      (* renvoie la liste des couples mot-position de la chaine s 
        exemples:
        - `alpha_blocks "hello world"` renvoie `[("hello", 0); (" ", 5); ("world", 6)]`
        - `alpha_blocks "a-b..."` renvoie 
          `[("a", 0); ("-", 1); ("b", 2); (".", 3); (".", 4); (".", 5)]`
    *)
    let result = ref [] in 
    let indice_non_alpha = ref (-1) in 
    let word = ref "" in 
    let id = ref 0 in 
    let i = ref 0 in
    let lg = String.length s in
    while !i  < lg do 
      indice_non_alpha := first_non_alpha_from s !i ;
      if !indice_non_alpha <> String.length s then 
        begin
          id := !i ;
          word := String.sub s !i (!indice_non_alpha - !i) ;
          if !i <> !indice_non_alpha then result := (!result)@[(!word,!id)];
          result := (!result)@[(String.make 1 (s.[!indice_non_alpha]),!indice_non_alpha)];
          i := !indice_non_alpha + 1 ; 
        end
      else 
        begin 
          word := String.sub s !i (!indice_non_alpha - !i) ;
          id := !i;
          result := (!result)@[(!word,!id)] ; 
          i := !indice_non_alpha ;
        end
    done;
    !result

  let encode voc s =
    let l = voc_size voc in
    let alpha_b = alpha_blocks s in
    let long = List.length alpha_b in
    let result = ref [] in
    let found = ref false in
    let taille = ref 0 in
    for i=0 to (long-1) do
      found := false;
      for j=0 to (l-1) do
        if (fst(List.nth alpha_b i) = fst(List.nth voc j)) then 
          begin
            result := !result@[snd(List.nth voc j)] ; 
            found := true ; 
          end
      done;
      if not !found then 
      begin 
        taille := (String.length s) - snd (List.nth alpha_b i) ;
        raise (EncodingError (String.sub s (snd(List.nth alpha_b i)) !taille ));
      end
    done;
    !result

  exception DecodingError of int

  let decode (voc:vocabulary) ids = 
    let result = ref "" in 
    let lg = List.length ids in
    let l = voc_size voc in 
    let found = ref false in
    for i=0 to (lg-1) do
      found := false;
      for j=0 to (l-1) do
        if (List.nth ids i)=snd(List.nth voc j) then
          begin
            result := !result^(fst(List.nth voc j));
            found := true ; 
          end
      done;
      if not !found then raise (DecodingError (List.nth ids i))
    done;
    !result

  let learn batch = 
  let result = ref [] in 
  let id = ref 0 in 
  let words = ref [] in 
  let len = List.length batch in 
  let lg = ref 0 in 
  let rec exist mot l = match l with 
    |[] -> false
    |(hd,_)::tl -> if hd=mot then true else false || (exist mot tl) 
  in
  for i=0 to len - 1 do 
    words := alpha_blocks (List.nth batch i);
    lg := List.length !words;
    for j=0 to !lg - 1 do
      if not( exist (fst(List.nth !words j)) !result ) then
        begin
          result := (!result)@[(fst(List.nth !words j) , !id)];
          id := !id + 1 ; 
        end;
    done;
  done;
  !result

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer