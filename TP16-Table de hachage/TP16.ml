(* Exercice 1 *)

type 'a tab_redim = {mutable nb: int ; mutable tab: 'a array} ;;

let creer_tab () = {nb = 0; tab = [| |]} ;;

let acces t i =
  if i<t.nb then
    t.tab.(i)
  else
    failwith "depassement d'indice"
;;

let modif t i x =
  if i<t.nb then
    t.tab.(i) <- x
  else
    failwith "depassement d'indice"
;;

let ajout t x =
  if t.nb = Array.length t.tab then
    begin (* on fait grossir le tableau *)
      let u=Array.make (2*t.nb+1) x in 
      for i=0 to t.nb-1 do (* on recopie les Ã©lÃ©ments *)
        u.(i) <- t.tab.(i)
      done ;
      t.nb <- t.nb + 1 ;
      t.tab <- u
    end
  else
    begin
      t.tab.(t.nb) <- x ;
      t.nb <- t.nb + 1
    end
;;

let suppr t =
  if t.nb > 0 then
    begin
      t.nb <- t.nb - 1 ;
      t.tab.(t.nb)
    end
  else
    failwith "tableau vide"
;;

(*test*)
let t = creer_tab () ;;

for i=0 to 9 do
  ajout t i
done ;;

acces t 3 ;;

for i=0 to 9 do
  print_int (suppr t) ; print_string " "
done ;;

(* Exercice 2 *)

type ('a, 'b) table_hachage = {hache: 'a -> int; donnees: ('a * 'b) list array };;

(* Q1 *)
let hachage_entier w k = k mod w ;;

(* hachage_entier 14 201 ;; *)

(* Q2 *)
let hachage_chaine w s=
  let x=ref 0 and p=ref 1 in
  for i=0 to String.length s - 1 do
    x:=(!x + (int_of_char s.[i])* !p) mod w ;
    p:= ( !p*128) mod w
  done ;
  !x
;;

hachage_chaine 12 "Bonjour !" ;;

hachage_chaine 12 "oh oui youpi dansons la carioca" ;;

(* version schÃ©ma de HÃ¶rner *)
let hachage_chaine w s =
  let x=ref 0 in
  for i=String.length s - 1 downto 0 do
    x:= ( !x * 128 + int_of_char s.[i]) mod w
  done ;
  !x
;;

hachage_chaine 12 "Bonjour !" ;;

hachage_chaine 12 "oh oui youpi dansons la carioca" ;;



(* Exercice 3 *)

(* Q1 *)
let creer_table h w={hache= h ; donnees=Array.make w []} ;;

creer_table (hachage_entier 5) 5 ;;

let petit_exemple = {
    hache = hachage_entier 3;
     donnees =
       [|[(15, "truc"); (468, "ocaml"); (498, "confinement"); (144, "TP")];
         [(1, "machin"); (154, "coucou")]; [(185, "info"); (512, "MPSI")]|]
    }
;;

(* Q2 *)
let recherche t k =
  let rec aux l = match l with
    | [] -> false
    | (k',e)::q -> k'=k || aux q
  in aux t.donnees.(t.hache k)
;;

recherche petit_exemple 498 ;;

recherche petit_exemple 499 ;;

(* Q3 *)
let element t k =
  let rec aux l = match l with
    | [] -> raise Not_found
    | (k',e)::q when k'=k -> e
    | _::q -> aux q
  in aux t.donnees.(t.hache k)
;;

element petit_exemple 498 ;;

element petit_exemple 499 ;;

(* Q4 *)
let ajout t k e =
  if not (recherche t k) then
    let hk=t.hache k in
    t.donnees.(hk) <- (k,e)::t.donnees.(hk)
;;

ajout petit_exemple 38 "Turing" ;;

petit_exemple.donnees.(2) ;;

(* Q5 *)
let suppression t k =
  let rec aux q = match q with
    | [] -> []
    | (k',e)::q when k'=k -> q
    | x::q -> x::(aux q)   
  in let hk=t.hache k in t.donnees.(hk) <- aux t.donnees.(hk) 
;;

suppression petit_exemple 498 ;;

petit_exemple.donnees.(0) ;;


(* Exercice 4 *)

type ('a, 'b) table_dyn = {hache: int -> 'a -> int ;
                           mutable taille: int ;
                           mutable donnees: ('a * 'b) list array} ;;

(* Q1 *)
let creer_table_dyn h w = {hache= h ; taille=0 ; donnees=Array.make w []} ;;

let recherche_dyn t k =
  let rec aux l = match l with
    | [] -> false
    | (k',e)::q -> k'=k || aux q
  in
  let w = Array.length t.donnees in
  let hk = t.hache w k in
  aux t.donnees.(hk) ;
;;


let element_dyn t k =
  let rec aux l = match l with
    | [] -> raise Not_found
    | (k',e)::q when k'=k -> e
    | _::q -> aux q
  in
  let w = Array.length t.donnees in
  let hk = t.hache w k in
  aux t.donnees.(hk) ;
;;

(* Q2 *)
let rearrange_dyn t = (* Ã  utliser lorsque qu'il y a trop d'Ã©lÃ©ments dans la lable *)
  let w = Array.length t.donnees in
  let nv_donnees = Array.make (2*w) [] in
  let nv_h = t.hache (2*w) in
  let rec aux l = match l with (* insÃ¨re les Ã©lÃ©ments de l dans le tableau nv_donnees *)
    | [] -> ()
    | (k,e)::q -> nv_donnees.(nv_h k) <- (k,e)::nv_donnees.(nv_h k) ; aux q
  in 
  for i=0 to w-1 do (*appelle aux sur toutes les listes du tableau donnees *)
    aux t.donnees.(i)
  done ;
  t.donnees <- nv_donnees
;;

(* Q3 *)
let ajout_dyn t k e =
  let w = Array.length t.donnees in
  if not (recherche_dyn t k) then
    begin
      let hk = t.hache w k in
      t.donnees.(hk) <- (k,e)::t.donnees.(hk) ;
      t.taille <- t.taille + 1 ;
      if t.taille > 2*w then rearrange_dyn t
    end 
;;

(* Q4 *)
(* En terme de complexitÃ© amortie (pour ne pas faire Ã§a trop souvent), il vaut
 * mieux diviser la largeur de la table par 2 lorsque la taille de la table
 * devient infÃ©rieure Ã  la moitiÃ© de sa largeur *)

let rearrange_bis_dyn t = (* Ã  utliser lorsque qu'il y a trop peu d'Ã©lÃ©ments dans la lable *)
  let w = Array.length t.donnees in
  let nv_donnees = Array.make (w/2) [] in
  let nv_h = t.hache (w/2) in
  let rec aux l = match l with (* insÃ¨re les Ã©lÃ©ments de l dans le tableau nv_donnees *)
    | [] -> ()
    | (k,e)::q -> nv_donnees.(nv_h k) <- (k,e)::nv_donnees.(nv_h k) ; aux q
  in 
  for i=0 to w-1 do (*appelle aux sur toutes les listes du tableau donnees *)
    aux t.donnees.(i)
  done ;
  t.donnees <- nv_donnees
;;

let suppression_dyn t k =
  let w = Array.length t.donnees in
  let hk = t.hache w k in
  let rec aux l = match l with
    | [] -> raise Not_found (* si l'Ã©lÃ©ment n'existe pas, on lÃ¨ve une exception *)
    | (k',e)::q when k'=k -> q
    | x::q -> x::(aux q)
  in try 
     t.donnees.(hk) <- aux t.donnees.(hk) ; (* qui fait sauter directement de cette ligne *)
     t.taille <- t.taille - 1 ; 
     if t.taille < w/2 then rearrange_bis_dyn t
  with Not_found ->  () (* Ã  cette ligne *)
;;