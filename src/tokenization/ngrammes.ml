let ngrammes (k: int) (l: 'a list) : 'a list list =
  let rec take k lst = match k, lst with
    |0,_ -> []
    |_,[] -> []
    |n, x :: xs -> x :: take (n-1) xs
  in
  if k < 1 then 
    invalid_arg"ngrammes"
  else
    let rec n_grm  l =
      match l with
      |[] -> []
      | x -> 
          if List.length x < k  then []
          else (take k x) :: n_grm  (List.tl x)
    in 
    n_grm  l