(* Type de nos polynomes *)
type poly = (int * float) list;;


(* Retourne le coefficient du polynome poly au degre i *)
let rec coeff_poly poly i =
  match poly with
  | [] -> 0.
  |(d,c)::poly when i = d -> c
  |a::poly -> coeff_poly poly i;;

(* Additionne deux polynomes poly1 et poly2 *)
let somme_poly poly1 poly2 =
  let rec aux poly1 poly2 tmp =
    match (poly1,poly2) with
    | ([],[]) -> List.rev tmp
    | ([],poly) -> List.rev_append tmp poly
    | (poly,[]) -> List.rev_append tmp poly
    | ((d,c)::t1,(d2,c2)::t2) when d = d2 ->
        let c3 = c +. c2 
        in if c3 = 0. then
          aux t1 t2 tmp  
        else
          aux t1 t2 ((d,c3)::tmp) 
    | ((d,c)::t1,(d2,c2)::t2) when d < d2 -> aux t1 poly2 ((d,c)::tmp)
    | ((d,c)::t1,(d2,c2)::t2) -> aux poly1 t2 ((d2,c2)::tmp)
  in aux poly1 poly2 [];;

(* Multiplie les coefficients de poly par a *)
let rec multCoeff poly a =
  match poly with
  | [] -> [] 
  | (d,c)::t -> (d,c*.a)::(multCoeff t a);;      

(* Renvoi le degre du polynome poly *)
let degre poly = 
  let rec aux poly max =
    match poly with
    | [] -> max 
    |(d,_)::t when d > max -> aux t d
    |(d,_)::t  -> aux t max 
  in aux poly 0;;

(* Multiplie le polynome poly par X^n  *)
let multXn poly n = 
  let rec aux poly tmp =
    match poly with
    | []  -> List.rev tmp 
    | (d,c)::t -> aux t ((d+n,c)::tmp) 
  in aux poly [];;  

(* Multiplie les coefficients de poly par a puis multiplie le resultat par X^n  *)
let multXn_coeff poly n coeff = 
  multXn (multCoeff poly coeff) n;;

(* Multiplication naive des deux polynomes poly1 et poly2 *)
let multNaive poly1 poly2 =
  let rec aux p acc =
    match p with
    | [] -> acc
    | (d,c)::t1 -> aux t1 (somme_poly (multXn_coeff poly2 d c) acc)
  in aux poly1 [];;

(* Coupe le polynome en deux au k-ieme element *)
let cut poly k = 
  let rec aux poly poly1 poly2 =
    match poly with
    | [] -> ((List.rev poly1),(List.rev poly2))
    | (d,c)::t when d < k -> aux t ((d,c)::poly1) poly2
    | (d,c)::t when d >= k -> aux t poly1 ((d-k,c)::poly2) 
    | _  -> failwith "error"
  in aux poly [] [];;

(* Fais la difference de deux polynomes *)
let diff_poly poly1 poly2 = 
  somme_poly (poly1) (multCoeff (poly2) (-.1.));;


(* SECTION KARATSUBA *)

(* Multiplie deux polynomes selon l'algorithme de Karatsuba revisité*)
let rec mult_poly_alpha poly1 poly2 a =
  if (degre poly1 = 0) || (degre poly2 = 0)
  then (multNaive poly1 poly2)
  else
    let m = degre poly1
    in let n = degre poly2
    in let k =
let mx = (max m n)
in if mx mod 2 = 0 then mx else mx+1
    in let (a0,a1) = cut poly1 (k/2)
    in let (b0,b1) = cut poly2 (k/2)
    in let c0 = mult_poly_alpha a0 b0 a
    in let c2 = mult_poly_alpha a1 b1 a
    in let u = mult_poly_alpha (somme_poly a0 (multCoeff a1 a)) (somme_poly b0 (multCoeff b1 a)) a
    in let c1 = multCoeff (diff_poly (diff_poly u c0) (multCoeff c2 (a**2.))) (1./.a)
    in somme_poly (somme_poly c0 (multXn c1 (k/2))) (multXn c2 k);;


(* Coupe le polynome en 3 au k-ieme element *)
let cut3 poly k =
  let rec aux p p1 p2 p3 =
    match p with
    | [] -> ((List.rev p1),(List.rev p2),(List.rev p3))
    | (d,c)::t when d < k -> aux t ((d,c)::p1) p2 p3
    | (d,c)::t when d < (2 * k) -> aux t p1 ((d - k,c)::p2) p3
    | (d,c)::t -> aux t p1 p2 ((d - (2*k),c)::p3)
  in aux poly [] [] [];;


(* Renvoi l'entier n tel que le degres maximum des polynomes soit <= 3n-1 *)
let find_cut poly1 poly2 =
    let rec aux degre1 degre2 nbCut = match degre1,degre2 with
    | d1,d2 when nbCut > d1 && nbCut > d2 -> nbCut/3
    | d1,d2 -> aux degre1 degre2 (nbCut+3)
    in aux (degre poly1) (degre poly2) 3;;



(* SECTION TOOM 3 *)

(* Multiplie deux polynomes selon l'algorithme de Toom-3*)
let toom3 polyA polyB alpha =	
    let rec aux polyA polyB = 
			let n = (find_cut polyA polyB) in
      if (degre polyA) < 3 || (degre polyB) < 3 then
        multNaive polyA polyB
      else
        let (p0, p1, p2) = cut3 polyA n
        and (q0, q1, q2) = cut3 polyB n in 
        let _R1 = aux (somme_poly p0 (somme_poly p1 p2))
            (somme_poly q0 (somme_poly q1 q2)) 
        and _Rm1 = aux (somme_poly (diff_poly p0 p1) p2)
            (somme_poly (diff_poly q0 q1) q2) in 
        let _A = multCoeff (somme_poly _R1 (multCoeff _Rm1 (-1.))) (1./.2.) 
        and r0 = aux p0 q0
        and r4 = aux p2 q2 
        and addPandQ = aux (somme_poly p0 (somme_poly (multCoeff p1 alpha) (multCoeff p2 (alpha**2.))))
            (somme_poly q0 (somme_poly (multCoeff q1 alpha) (multCoeff q2 (alpha**2.)))) in 
        let r2 = diff_poly (diff_poly ((multCoeff(somme_poly _R1 _Rm1) (1./.2.))) r0) r4 in 
        let _B = diff_poly (diff_poly (diff_poly addPandQ r0) (multCoeff r2 (alpha**2.))) (multCoeff r4 (alpha**4.)) in
        let r3 = multCoeff (diff_poly _B (multCoeff _A alpha))  (1./.((alpha**3.) -. alpha)) in
        let r1 = diff_poly _A r3 in 
        somme_poly r0 (somme_poly (multXn r1 n) (somme_poly (multXn r2 (2*n)) (somme_poly (multXn r3 (3*n)) (multXn r4 (4*n)))))
    in aux polyA polyB ;;


(* Créer une liste de n nombres de 0 à 10 puis de 10 à d *)
let create_random_list n d =
  let rec aux l k = 
    match k with 
    | k when k < 11 -> aux (l @ [k]) (k+1) 
    | k when k < n -> aux (l @ [((Random.int d) +10)]) (k+1)
    | k -> l 
  in aux [] 1 ;;

(* Créer un polynome de degre degre *)
let create_poly degre = 
  let rec aux poly degre = 
    let rand = (Random.int 10) in 
    match degre with
    | degre when degre = 0 -> poly
    | degre when rand = 0 -> aux poly (degre-1) 
    | degre -> aux ((degre,float_of_int rand)::poly) (degre-1)
  in aux [] degre;;

(* Créer 10 polynomes de degre degre *)
let create_ten_poly degre = 
  let rec aux l cpt = 
    match cpt with 
    | cpt when cpt = 10 -> l
    | cpt -> aux (((create_poly degre)@[])::l) (cpt+1)
  in aux [] 0;;

(* Créer 10 polynomes à partir d'une liste de polynomes *)
let create_ten_poly_from_list l =
  let rec aux l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> aux t ((create_ten_poly h)::l2)
  in aux l [];;

(* Créer une liste de n nombres de 0 à 10 puis de 10 à d puis créer 10 polynomes à partir de cette liste*)
let create_all_poly n d =
  let l = create_random_list n d in
  create_ten_poly_from_list l;;

(* Renvoi le temps d'execution de la fonction f *)
let time f = 
  let t = Sys.time() in
  let res = f () in
  (Sys.time() -. t);;

(* Retourne deux elements aléatoirement choisis dans la liste l *)
let return_pair l =
  let rand1 = (Random.int 9) and  rand2 = (Random.int 9) in 
  let rec aux l = match l with 
    | l when (List.length l < 2) -> ([],[])
    | l -> (List.nth l rand1, List.nth l rand2) 
  in
  aux l;;

(* Retourne une list de paires de polynomes tirés au hasard dans une liste de liste de polynomes *)
let pairs_from_list_poly listPoly = 
  let rec aux listPoly listPairs =
    match listPoly with
    | [] -> listPairs
    | h::t -> aux t ((return_pair h)::listPairs)
  in aux listPoly [];;

(* Retourne une liste des temps d'execution de la multiplication des polynomes de la liste des paires l en utilisant Toom3 *)
let eval_poly_from_couple_list_toom3 l alpha = 
  let rec aux l timeList = match l with
    | [] -> timeList
    | h::t -> aux t ((time (fun () -> toom3 (fst h) (snd h) alpha))::timeList)
  in aux l [];;
(* Retourne une liste des temps d'execution de la multiplication des polynomes de la liste des paires l en utilisant Karatsuba revisité *)
let eval_poly_from_couple_list_karatsuba l alpha= 
  let rec aux l timeList = match l with
    | [] -> timeList
    | h::t -> aux t ((time (fun () -> mult_poly_alpha (fst h) (snd h) alpha))::timeList)
  in aux l [];;
(* Retourne une liste des temps d'execution de la multiplication des polynomes de la liste des paires l en utilisant la multiplication naive *)
let eval_poly_from_couple_list_naive l=
  let rec aux l timeList = match l with
    | [] -> timeList
    | h::t -> aux t ((time (fun () -> multNaive (fst h) (snd h) ))::timeList)
  in aux l [];;

(* Retourne la moyenne des elements d'une liste l *)
let mean_from_list_sum l =
	let rec aux l sum nb = match l with
	| [] -> sum/.nb
	| h::t -> aux t (sum+.h) (nb+.1.) in
	aux l 0. 0.;;

(* Retourne la liste des temps d'execution de la multiplication utilisant Toom3 sur une liste de n degres allant de 0 a d *)
let eval_toom3 n d alpha =
  let l = create_all_poly n d in
  let l2 = pairs_from_list_poly l in
  eval_poly_from_couple_list_toom3 l2 alpha;;

(* Retourne la liste des temps d'execution de la multiplication utilisant karatsuba revisité sur une liste de n degres allant de 0 a d *)
let eval_karatsuba n d alpha=
  let l = create_all_poly n d in
  let l2 = pairs_from_list_poly l in
  eval_poly_from_couple_list_karatsuba l2 alpha ;;

(* Retourne la moyenne de la liste des temps d'execution de la multiplication utilisant la multiplication naive sur une liste de n degres allant de 0 à d *)
let eval_naive n d =
  let l = create_all_poly n d in
  let l2 = pairs_from_list_poly l in
  eval_poly_from_couple_list_naive l2;;
	
(* Retourne la moyenne des temps d'execution pour la multiplication naive*)
let mean_naive = mean_from_list_sum (eval_naive 20 5000);;
(* Retourne la moyenne des temps d'execution pour l'algorithme de Karatsuba revisité*)
let mean_karatsuba = mean_from_list_sum (eval_karatsuba 20 5000 5.);;
(* Retourne la moyenne des temps d'execution pour l'algorithme de Toom3*)
let mean_toom3 = mean_from_list_sum (eval_toom3 20 5000 5.);;

(* !!!  EXEMPLES  !!! *)

  (* Vous pouvez tester en modifiant les valeurs:  20 (nombre de polynome à tiré au sort), 5000 (degré maximum), 5. (alpha arbitraire). *)
