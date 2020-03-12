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

(* funkcije fold ... *)

let zlozi_levo f z xs =
  let rec aux acc = function 
    | [] -> acc
    | x :: xs' -> aux (f acc x) xs'
  in
  aux z xs 

let rec zlozi_levo_no_acc f z xs = match xs with
  | [] -> z
  | x :: xs' -> zlozi_levo_no_acc f (f z x) xs' 

let rec zlozi_desno f xs z = match xs with
  | [] -> z
  | x :: xs' -> f x (zlozi_desno f xs' z) 

(* primeri *)

let rec dolzina = function
  | [] -> 0
  | x :: xs -> 1 + dolzina xs

let dolzina_w_fold xs = zlozi_desno (fun _ -> (fun y -> y + 1)) xs 0 
(*                    = zlozi_desno (fun _ acc -> 1 + acc) xs 0
*)

let vsota_w_fold xs = zlozi_desno (+) xs 0
let produkt_w_fold xs = zlozi_desno ( * ) xs 1
let map_w_fold g xs = zlozi_desno (fun y -> (fun ys -> g y :: ys)) xs []  
(*                  = zlozi_desno (fun y ys -> g y :: ys) xs []
*)

(* vsote *)

type geometrijski_objekti =
  | Tocka
  | Krog of float
  | Pravokotnik of (float * float)

let povrsina (objekt : geometrijski_objekti) =
  match objekt with
  | Tocka -> 0.
  | Krog r -> 3.14 *. r *. r
  | Pravokotnik (a, b) -> a *. b 

let moji = [Tocka; Pravokotnik (3., 4.)]

type 'a tree = 
  | Empty
  | Node of 'a node
and 'a node = { left: 'a tree; value: 'a; right: 'a tree }

type 'a list_ = 
  | Nil
  | Cons of 'a * 'a list_