(* Como testar? No TryOcaml inserir um ou mais testes num campo com # e depois em Eval phrase
Ou inserir no próprio cõdigo e ver os reusltados quando fizer Eval Code*)


(* Teste de operações básicas *)
let _ = int_bse (Num 5)  (* Esperado: 5 : int *)
let _ = int_bse (Bool true)  (* Esperado: true : bool *)
let _ = int_bse (Binop (Sum, Num 5, Num 3))  (* Esperado: 8 : int *)

(* Teste de condicional *)
let _ = int_bse (If (Bool true, Num 10, Num 20))  (* Esperado: 10 : int *)

(* Teste de função simples *)
let _ = int_bse (App (Fn ("x", TyInt, Binop (Sum, Var "x", Num 1)), Num 5))  (* Esperado: 6 : int *)

(* Teste de let rec para função recursiva *)
let _ = int_bse (
  LetRec ("fact", TyFn (TyInt, TyInt),
    Fn ("n", TyInt,
      If (Binop (Eq, Var "n", Num 0), 
        Num 1, 
        Binop (Mult, Var "n", App (Var "fact", Binop (Sub, Var "n", Num 1))))),
    App (Var "fact", Num 5)
  )
)  (* Esperado: 120 : int *)

(* Teste de listas *)
let _ = int_bse (List (Num 1, List (Num 2, Nil TyInt)))  (* Esperado: 1 :: 2 :: nil : list int *)

(* Teste para 'nothing' *)
let _ = int_bse (Nothing TyInt)  (* Esperado: nothing : maybe int *)

(* Teste para 'just' com um número *)
let _ = int_bse (Just (Num 42))  (* Esperado: just 42 : maybe int *)

(* Teste de 'match' com 'nothing' *)
let _ = int_bse (MatchWithNothing (Nothing TyInt, Num 0, "x", Var "x"))  (* Esperado: 0 : int *)

(* Teste de 'match' com 'just' *)
let _ = int_bse (MatchWithNothing (Just (Num 5), Num 0, "x", Binop (Sum, Var "x", Num 1)))  (* Esperado: 6 : int *)

(* Teste para 'nil' *)
let _ = int_bse (Nil TyInt)  (* Esperado: nil : list int *)

(* Teste de construção de lista com '::' *)
let _ = int_bse (List (Num 1, Nil TyInt))  (* Esperado: 1 :: nil : list int *)

(* Teste de 'match' com lista vazia *)
let _ = int_bse (MatchWithNil (Nil TyInt, Num 0, "x", "xs", Var "x"))  (* Esperado: 0 : int *)

(* Teste de 'match' com lista não vazia *)
let _ = int_bse (MatchWithNil (List (Num 1, Nil TyInt), Num 0, "x", "xs", Binop (Sum, Var "x", Num 1)))  (* Esperado: 2 : int *)

(* Teste de 'pipe' com função de incremento *)
let _ = int_bse (Pipe (Num 1, Fn ("x", TyInt, Binop (Sum, Var "x", Num 1))))  (* Esperado: 2 : int *)

(* Teste de 'pipe' com função de duplicação *)
let _ = int_bse (Pipe (Num 2, Fn ("x", TyInt, Binop (Mult, Var "x", Num 2))))  (* Esperado: 4 : int *)

(* Teste para tipo Maybe *)
let test_maybe_type () =
  let e = 
    (* Testa um caso com Just e MatchWithNothing *)
    MatchWithNothing(
      Just(Num 42), 
      Num 0, 
      "x", 
      Binop(Sum, Var "x", Num 10)
    )
  in
  int_bse e (* Espera-se que o resultado seja 52 *)

(* Executa o teste *)
let () = test_maybe_type ()


(*(fn x:int => (fn y:int => x+y)) 12 4 *) (* Esperado: 16 : int *)
let soma = int_bse (App(Fn("x", TyInt, App(Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), Num 12)), Num 4)) 
  
                                             
  (*(fn x:bool => (fn y:int => x+y)) true 4 *) (* Esperado: erro de tipo *)
let somaErro = int_bse (App(Fn("x", TyInt, App(Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), Bool true)), Num 4))
    
    
(*(fn x:int => (fn y:int => x+y))*)     (* Esperado: (int --> (int --> int))*)
let somaFuncao = int_bse (Fn("x", TyInt, Fn("y", TyInt, Binop(Sum, Var "x", Var "y"))))
    
(*
let rec lookup: int -> (int * int) list -> maybe in  = 
  fn k:int => fn l: (int*int) list =>
    match l with 
      nil      -> nothing 
    | x :: xs -> if (fst x) = k 
        then just (snd x)
        else lookup k xs  in 

let base_dados : (int * int) list =  [(1,10), (2,20), (3,30), (4,40), (5,50)] in

let key:int = 3 in 

match lookup key base_dados with
  noting -> 0
| just n -> n
   *)
  
let lookupExpr = 
  LetRec
    ("lookup",
     TyFn(TyInt, TyFn(TyList(TyPair(TyInt,TyInt)), TyMaybe(TyInt))),
     Fn("k", TyInt,
        Fn("l", TyList(TyPair(TyInt, TyInt)),
           MatchWithNil(Var "l", Nothing(TyInt), "x", "xs",
                        If (Binop (Eq, Fst(Var "x"), Var "k"),
                            Just (Snd (Var "x")),
                            App(App(Var "lookup", Var "k"), Var "xs"))))),
     Let
       ("base_dados",
        TyList(TyPair(TyInt, TyInt)),
        Cons(Pair(Num 1, Num 10),
             Cons(Pair(Num 2, Num 20),
                  Cons(Pair(Num 3, Num 30),
                       Cons(Pair(Num 4, Num 40),
                            Cons(Pair(Num 5, Num 50), Nil(TyPair(TyInt, TyInt))))))),
        Let
          ("key",
           TyInt,
           Num 3,
           MatchWithNothing(App(App(Var "lookup", Var "key"), Var "base_dados"),
                            Nothing(TyInt),
                            "n",
                            (Fn ("n", TyInt, Num 0))))))
  
let _ = int_bse(lookupExpr)

(*
 let rec list_max: int list -> maybe int  = 
fn l:int list =>
  match l with 
  | nil -> nothing
  | h :: t -> (
      match list_max t with
      | noting -> just h
      | just m -> just (if h >= m then h else m))
let base_dados : (int) list =  [10, 20, 30, 40, 50] in
match list_max base_dados with
  nothing -> 0
| just n -> n
*)
(*Esperado: maior número, 50*)
let listMaxExpr = 
  LetRec(
    "list_max",
    TyFn(TyList (TyInt), TyMaybe(TyInt)),
    Fn("l", TyList(TyInt),
       MatchWithNil(Var "l", Nothing(TyInt), "h", "t",
                    MatchWithNothing(App(Var "list_max", Var "t"),
                                     Just(Var "h"),
                                     "m",
                                     Just(
                                       If (Binop (Geq, Var "h", Var "m"),
                                           Var "h",
                                           Var "m"))))),
    Let
      ("base_dados",
       TyList(TyInt),
       Cons(Num 10,
            Cons(Num 50,
                 Cons(Num 30,
                      Cons(Num 40,
                           Cons(Num 50, Nil(TyInt)))))),
       MatchWithNothing(
         App(Var "list_max", Var "base_dados"),
         Num 0,
         "n",
         Var "n"
       )
      )
  )
    
let _ = int_bse(listMaxExpr)
