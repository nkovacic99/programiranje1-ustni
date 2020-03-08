let a = [2;4;1;7]

let max_v_sez = function
  | [] -> None
  | x :: xs -> Some (List.fold_left max x xs) 

let liha = [|0;1;2;3;4|]
let soda = [|0;1;2;3|]

let zamenjaj t i j = 
  let x = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- x

let obrni_na_mestu tabela = 
  let n = Array.length tabela in
  for i = 0 to (n / 2 - 1) do
    zamenjaj tabela i (n - i - 1)
  done

let obrni_novo tabela = 
  let n = Array.length tabela in
  Array.init n (fun i -> tabela.(n - i - 1))

let obrni_novo' tabela = 
  let kopija = Array.copy tabela in
  obrni_na_mestu kopija;
  kopija
  

let fish_yates tabela =
  let n = Array.length tabela in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in (* i + 1 je zato ker Random.int izbira inte do meje izkljuceno *)
    zamenjaj tabela i j
  done

let razdeli pivot sez =
  let rec aux sez manjsi vecji = match sez with
    | [] -> (manjsi, vecji)
    | x :: xs when x <= pivot -> aux xs (x :: manjsi) vecji
    | x :: xs -> aux xs manjsi (x :: vecji)
  in aux sez [] [] 

let rec quick_sort sez = match sez with
  | [] -> []
  | x :: xs -> 
    let sez1, sez2 = razdeli x xs in
    quick_sort sez1 @ (x :: quick_sort sez2)

let test = [|3;2;4;6;1;5|]

let bubble_sort tabela = 
  let n = Array.length tabela in
  if n > 1 then
  for k = n - 2 downto 0 do
    for i = 0 to k do
      if tabela.(i) > tabela.(i + 1) then zamenjaj tabela i (i + 1)
    done
  done

let insertion_sort tabela = 
  let n = Array.length tabela in
  if n > 1 then
  for k = 1 to n - 1 do
    for i = k downto 1 do
      if tabela.(i - 1) > tabela.(i) then zamenjaj tabela i (i - 1)
    done
  done

let selection_sort tabela = 
  let n = Array.length tabela in
  if n > 1 then
  for k = 0 to n - 1 do
    let m = [|k|] in
    for i = k to n - 1 do
      if tabela.(m.(0)) > tabela.(i) then 
        m.(0) <- i; 
    done;
    zamenjaj tabela m.(0) k
  done