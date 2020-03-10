```ocaml
let rec (@) xs ys =
  match xs with
  | [] -> ys
  | x :: xs' -> x :: (xs' @ ys)
```

```ocaml
let rec obrni = function
  | [] -> []
  | x :: xs -> obrni xs @ [x]
```

```ocaml
let rec dolzina = function
  | [] -> 0
  | _ :: xs -> 1 + dolzina xs
```


+ dolzina (xs @ ys) = dolzina xs + dolzina ys

Dokaz
	
	Baza:

	dolzina ([] @ ys) =
	dolzina ys =
	0 + dolzina ys =
	dolzina [] + dolzina ys

	korak:
	i.p. je dolzina (xs @ ys) = dolzina xs + dolzina ys

	dolzina ((x :: xs') @ ys) =
	dolzina (x :: (xs' @ ys)) =
	1 + dolzina (xs' @ ys) = i.p.
	1 + dolzina xs' + dolzina ys =
	dolzina (x :: xs') + dolzina ys.
													qed

+ xs @ (ys @ zs) = (xs @ ys) @ zs

Dokaz

	Baza:
	[] @ (ys @ zs) =
	ys @ zs =
	([] @ ys) @ zs

	Korak:
	i.p. je xs @ (ys @ zs) = (xs @ ys) @ zs

	(x :: xs) @ (ys @ zs) = 
	x :: (xs @ (ys @ zs)) = i.p.
	x :: ((xs @ ys) @ zs) =
	(x :: (xs @ ys)) @ zs = (po def @)
	((x :: xs) @ ys) @ zs (se enkrat po def @)
												qed

+ obrni (xs @ ys) = obrni ys @ obrni xs

Dokaz

	Baza:
	obrni ([] @ ys) =
	obrni ys =
	(obrni ys) @ []

	Korak:
	i.p. je obrni (xs @ ys) = obrni ys @ obrni xs

	obrni ((x :: xs) @ ys) =
	obrni (x :: (xs @ ys)) =
	obrni (xs @ ys) @ [x] = i.p.
	(obrni ys @ obrni xs) @ [x] =
	obrni ys @ (obrni xs @ [x]) = (po def obrni)
	obrni ys @ obrni (x :: xs)

+ obrni (obrni xs) = xs
  
Dokaz

	Baza:
	obrni (obrni []) =
	obrni [] =
	[]

	Korak:
	obrni (obrni (x :: xs)) =
	obrni (obrni xs @ [x]) =
	obrni [x] @ obrni (obrni xs) = i.p. + se par vrstic
	x :: xs

```ocaml
let obrni' =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux []
```
```ocaml
let rec obrni = function
  | [] -> []
  | x :: xs -> obrni xs @ [x]
```

+ obrni xs = obrni' xs

Dokaz

	Baza:
	obrni [] =
	[] =
	aux [] [] =
	obrni' []

	Korak:
	i.p. je obrni xs = obrni' xs = aux [] xs

	obrni' (x :: xs) =
	aux [] (x :: xs) =
	aux (x :: []) xs =
	aux [x] xs = 

		(*)

	aux (obrni xs @ [x]) [] =
	obrni xs @ [x] =
	obrni (x :: xs)

	pomožni dokazek

		aux acc xs = aux ((obrni xs) @ acc) []
		- - - - - - - - - - - - - - - - - - - 

		za xs = [] je to baza, ki očinto velja

		Korak:

		aux acc (x :: xs) =
		aux (x :: acc) xs = i.p.
		aux ((obrni xs) @ (x :: acc)) [] = 
		(obrni xs) @ (x :: acc) =
		(obrni xs) @ ([x] @ acc) =
		(obrni xs @ [x]) @ acc =
		(obrni (x :: xs)) @ acc =
		aux ((obrni (x :: xs)) @ acc) []

		(*) tu vzamemo acc = [x] v podčrtkani enakosti.
