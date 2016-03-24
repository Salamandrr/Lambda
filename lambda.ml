(*Exercice 1*)
(*1*)
let rec union l1 l2 = match l1 with
	[] -> l2
	| t::r -> if List.mem t l2 then union r l2 else t::union r l2
;;

(*2*)
let rec inter l1 l2 = match l1 with
	[] -> []
	| t::r -> if List.mem t l2 then t::inter r l2 else inter r l2
;;

(*Exercice 2*)
(*1*)
type terme = 
	Cst of string
	| Var of string
	| Abs of string * terme
	| App of terme * terme
;;

(*2*)
(*a*)
let i = Abs ("x", Var "x");;

(*b*)
let a = App (i, Var  "a");;

(*c*)
let k = Abs ("x", Abs ("y", Var "x"));;

(*d*)
let s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))));;

(*e*)
let delta = App (Abs ("x", Var "x"), Var "x");;

(*f*)
let omega = App (delta, delta);;

(*3*)
let rec print_terme t =
	match t with
	Cst c -> print_string(c)
	| Var x -> print_string(x)
	| Abs (x,e) -> print_string("(\\" ^ x ^ "."); print_terme(e); print_string(")")
	| App(t1,t2) -> print_string("("); print_terme(t1); print_string(" "); print_terme(t2); print_string(")")
;;


(*Exercice 3*)
(*1*)
let rec remove l x=
    match l with
    |t::r->if t=x then remove r x else t::(remove r x)
    |[]->[]
;;

let rec fv t =                                                                                                                          
    match t with
    |Var x -> [x]
    |Cst c -> []
    |App (m,n)->union(fv m)(fv n)
    |Abs (x,m)->remove(fv m) x
;;


(*2*)
let rec bv t =
    match t with
    |Var x ->[]
    |Cst c ->[]
    |App (m,n) -> union(bv m)(bv n)
    |Abs (x,m)->union[x](bv m)
;;

(*3*)
let rec vars t =
    match t with
    |Var x ->[x]
    |Cst c ->[]
    |App (m,n) -> union (vars m)(vars n)
    |Abs (x,m) -> union [x] (vars m)
;;

(*4*)
let variable_fraiche l =
    let rec compteur i=
        if (List.mem (String.make 1 (Char.chr i)) l)
        then compteur (i+1)
        else (String.make 1 (Char.chr i))
    in compteur 97;;



(*Exercice 4*)
(*1*)
let rec subst t x t2 =
    if (List.mem x (fv t)) then
        match t with
        |Cst c -> Cst c
        |Var v -> if v=x then t2 else Var v
        |Abs (v,m) -> Abs (v,subst m x t2)
        |App (m,n) -> App (subst m x t2,subst n x t2)
    else t
;;

(*2*)
let rec alpha t x y =
       match t with
       |Cst c -> Cst c
       |Var v -> Var v
       |Abs (v,m) -> if v=x then Abs (y,subst m x (Var(y))) else Abs (v,alpha m x y)
       |App (m,n) -> App (alpha m x y,alpha n x y)
;;

(*Exercice 5*)
(*1*)
let rec est_forme_normale t = 
    match t with
    |Var x -> true
    |Cst c -> true
    |App (Abs _,_) -> false
    |App (m,n) -> est_forme_normale m && est_forme_normale n
    |Abs (_,e) -> est_forme_normale e
;;

(*2*)
let rec etape_NOR t = 
    let rec pbvar x m n =
        if (List.mem x (bv m) && List.mem x (fv n)) 
        then pbvar x (alpha m x (variable_fraiche (union (vars m) (vars n)))) n 
        else subst m x n
    in
    match t with 
    |Var x -> Var x
    |Cst c -> Cst c
    |App (Abs (x,m),n) -> pbvar x m n
    |App (m,n) ->
            let nor = etape_NOR n in
            if n = nor then App (etape_NOR m,n)
            else App (m,nor)
    |Abs (x,e) -> Abs (x,etape_NOR e)
;;
(*
let rec pbvar x m n =
    if (List.mem x (bv m) && List.mem x (fv n)) 
    then pbvar x (alpha m x (variable_fraiche (union (vars m) (vars n)))) n 
    else subst m x n

let rec etape_NOR t =
    match t with 
    |Var x -> Var x
    |Cst c -> Cst c
    |App (Abs (x,m),n) -> subst m x n
    |App (m,n) -> if n = etape_NOR n
                  then 
                    if m = etape_NOR m
                    then App (m,n)
                    else App(etape_NOR m,n)
                  else
                      if m = etape_NOR m
                      then App(m,etape_NOR n)
                      else App(etape_NOR m,etape_NOR n)
    |Abs (x,e) -> Abs (x,etape_NOR e)
;;
*)

(*3*)
let rec normalise t =
    if est_forme_normale t then t
    else normalise (etape_NOR t)
;;


(*Exercice 6*)
(*1*)
let vrai=Abs ("x",Abs ("y",Var "x"));;
let faux=Abs ("x",Abs ("y",Var "y"));;
let cond=Abs ("c",Abs ("v",Abs ("f",App (App (Var "c",Var "v"),Var "f"))));;

let cond_vrai=normalise (App (App (App (cond,vrai),Var "u"),Var "v"));;
let cond_faux=normalise (App (App (App (cond,faux),Var "u"),Var "v"));;

(*2*)
let successeur=Abs ("n",Abs ("f",Abs ("x",App (Var "f",App (App (Var "n",Var "f"),Var "x")))));;
let addition=Abs ("n",Abs ("m",Abs ("f",Abs ("x",App (App (Var "n",Var "f"),App (App (Var "m",Var "f"),Var "x"))))));;
let multiplication=Abs ("n",Abs ("m",Abs ("f",App (Var "m",App (Var "n",Var "f")))));;

let zero=Abs ("f",Abs ("x",Var "x"));;
let un=Abs ("f",Abs ("x",App (Var "f",Var "x")));;
let deux=Abs ("f",Abs ("x",App (Var "f",App (Var "f",Var "x"))));;

let somme_un_deux=normalise (App (App (addition,un),deux));;
print_terme somme_un_deux;;
let successeur_zero=normalise (App (successeur,zero));;
print_terme successeur_zero;;
let multiplication_deux_deux=normalise (App (App (multiplication,deux),deux));;
print_terme multiplication_deux_deux;;

(*3*)
let combinateur_de_pt_fixe_de_turing=App (Abs ("x",Abs ("y",App(Var "y",App (App (Var "x",Var "x"),Var "y")))),Abs ("x",Abs ("y",App (Var "y",App (App (Var "x",Var "x"),Var "y")))));;
print_terme combinateur_de_pt_fixe_de_turing;;
let fst=Abs ("t",App (Var "t",vrai));;
let snd=Abs ("t",App (Var "t",faux));;
let pair=Abs ("x",Abs ("y",Abs("t",App(App(Var "t",Var "x"),Var "y"))));;
let iszero=Abs ("n",App(App (Var "n",Abs ("x",faux)),vrai));;

let pred =Abs("n",Abs("f",Abs("x",App(fst,App(App((Var "n"),Abs("p",App(App(pair,App(snd,(Var "p"))),App((Var "f"),App(snd,(Var "p")))))),Abs("t",App(App((Var "t"),(Var "x")),(Var "x"))))))));;

let h=Abs ("f",Abs ("n",(App (App (cond,App (iszero,Var "n")),App (un,App (App (multiplication,Var "n"),App (Var "f",App (pred,Var "n"))))))));;
let fac=App (combinateur_de_pt_fixe_de_turing,h);;

(* let fac_2=normalise (App(fac,deux));; *)


