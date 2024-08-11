(* Como testar? No TryOcaml inserir um ou mais testes num campo com # e depois em Eval phrase
Ou inserir no próprio cõdigo e ver os reusltados quando fizer Eval Code*)


(* Teste de operações básicas *)
let _ = int_bse (Var "x")  (* Esperado: erro de tipo - variavel nao declarada:x *)
let _ = int_bse (Num 5)  (* Esperado: 5 : int *)
let _ = int_bse (Bool true)  (* Esperado: true : bool *)
let _ = int_bse (Binop (Sum, Num 5, Num 3))  (* Esperado: 8 : int *)
let _ = int_bse (Binop (Eq, Num  7, Num 4))  (* Esperado:  false : bool *)
let _ = int_bse (Binop (Lt, Bool true, Bool false))  (* Esperado: erro de tipo - operando nao é do tipo int *)
let _ = int_bse (Pair (Num 5, Num 2))  (* Esperado: (5,2) : (int * int ) *)
let _ = int_bse (Fst (Pair (Num 1, Num 3)))  (* Esperado: 1 : int *)
let _ = int_bse (Fst (Num 1))  (* Esperado: erro de tipo - fst espera tipo par *)
let _ = int_bse (Snd (Pair (Bool false, Bool true)))  (* Esperado: true : bool *)
let _ = int_bse (Snd (Bool false))  (* Esperado: erro de tipo - snd espera tipo par *)

(* Teste de condicional *)
let _ = int_bse (If (Bool true, Num 10, Num 20))  (* Esperado: 10 : int *)
let _ = int_bse (If (Bool false, Num 10, Num 20))  (* Esperado: 20 : int *)
let _ = int_bse (If (Num 0, Num 10, Num 20))  (* Esperado: erro de tipo - condição de IF não é do tipo bool *)
let _ = int_bse (If (Bool true, Bool false, Num 20))  (* Esperado: erro de tipo - then/else com tipos diferentes *)

(* Teste de função *)
let _ = int_bse (Fn ("x", TyInt, Binop (Sum, Var "x", Num 1)))  (* Esperado: int --> int *)
let _ = int_bse (Fn ("x", TyInt, Binop (Lt, Var "x", Num 1)))  (* Esperado: int --> bool *)    
let _ = int_bse (Fn("x", TyInt, Fn("y", TyInt, Binop(Sum, Var "x", Var "y")))) (*(fn x:int => (fn y:int => x+y))*)  (* Esperado: (int --> (int --> int))*)

(* Teste de aplicação *)
let _ = int_bse (App (Fn ("x", TyInt, Binop (Sum, Var "x", Num 1)), Num 5))  (* Esperado: 6 : int *)
let _ = int_bse (App(Fn("x", TyInt, App(Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), Num 12)), Num 4)) (*(fn x:int => (fn y:int => x+y)) 12 4 *) (* Esperado: 16 : int *)
let _ = int_bse (App (Bool false, Bool true)) (* Esperado: erro de tipo - tipo função era esperado *)
let _ = int_bse (App (Fn ("x", TyInt, Binop (Lt, Var "x", Num 1)), Bool true)) (* Esperado: erro de tipo - tipo argumento errado *)
let _ = int_bse (App(Fn("x", TyInt, App(Fn("y", TyInt, Binop(Sum, Var "x", Var "y")), Bool true)), Num 4)) (*(fn x:bool => (fn y:int => x+y)) true 4 *) (* Esperado: erro de tipo - tipo argumento errado*)

(* Teste de let *)
let _ = int_bse (Let ("x", TyInt, Num 10, Binop (Sum, Var "x", Num 20))) (* Esperado: 30 : int *)
let _ = int_bse (Let ("x", TyInt, Bool true, Binop (Sum, Var "x", Num 20))) (* Esperado: erro de tipo - expressão não é do tipo declarado em Let *)

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

let _ = int_bse (
  LetRec ("e_par", TyFn (TyInt, TyBool),
    Fn ("n", TyInt,
      If (Binop (Eq, Var "n", Num 0), 
          Bool true,
          If (Binop (Eq, Var "n", Num 1), 
            Bool false,
            App (Var "e_par", Binop (Sub, Var "n", Num 2))))),
    App (Var "e_par", Num 4)
  )
) (* Esperado: true : bool *)

let _ = int_bse (
  LetRec ("fact", TyFn (TyInt, TyInt),
    Fn ("n", TyInt,
      If (Binop (Eq, Var "n", Num 0), 
        Bool true, 
        Bool false)),
    App (Var "fact", Num 5)
  )
) (* Esperado: erro de tipo - tipo da função recursiva é diferente do declarado *) 

(* Teste para 'nothing' *)
let _ = int_bse (Nothing TyInt)  (* Esperado: nothing : maybe int *)

(* Teste para 'just' com um número *)
let _ = int_bse (Just (Num 42))  (* Esperado: just 42 : maybe int *)

(* Teste de 'match' com 'nothing' *)
let _ = int_bse (MatchWithNothing (Nothing TyInt, Num 0, "x", Var "x"))  (* Esperado: 0 : int *)

(* Teste de 'match' com 'just' *)
let _ = int_bse (MatchWithNothing (Just (Num 5), Num 0, "x", Binop (Sum, Var "x", Num 1)))  (* Esperado: 6 : int *)

(* Teste de 'match' com erros de tipo *)
let _ = int_bse (MatchWithNothing (Num 1, Num 0, "x", Var "x"))  (* Esperado: erro de tipo - match espera tipo maybe *)
let _ = int_bse (MatchWithNothing (Nothing TyInt, Num 0, "x", Bool false))  (* Esperado: erro de tipo - tipos diferentes em match *)

(* Teste para 'nil' *)
let _ = int_bse (Nil TyInt)  (* Esperado: nil : list int *)

(* Teste de listas *)
let _ = int_bse (List (Num 1, Nil TyInt))  (* Esperado: 1 :: nil : list int *)
let _ = int_bse (List (Num 1, List (Num 2, Nil TyInt)))  (* Esperado: 1 :: 2 :: nil : list int *)
let _ = int_bse (List (Num 1, List (Bool true, Nil TyBool)))  (* Esperado: erro de tipo - tipos diferentes em lista *)

(* Teste de 'match' com lista vazia *)
let _ = int_bse (MatchWithNil (Nil TyInt, Num 0, "x", "xs", Var "x"))  (* Esperado: 0 : int *)

(* Teste de 'match' com lista não vazia *)
let _ = int_bse (MatchWithNil (List (Num 1, Nil TyInt), Num 0, "x", "xs", Binop (Sum, Var "x", Num 1)))  (* Esperado: 2 : int *)

(* Teste de 'match' com erros de tipo *)
let _ = int_bse (MatchWithNil (Nothing TyInt, Num 0, "x", "xs", Var "x"))  (* Esperado: erro de tipo - match espera tipo lista *)
let _ = int_bse (MatchWithNil (List (Num 1, Nil TyInt), Num 0, "x", "xs", Bool true))  (* Esperado: erro de tipo - tipos diferentes em match *)

(* Teste de 'pipe' com função de incremento *)
let _ = int_bse (Pipe (Num 1, Fn ("x", TyInt, Binop (Sum, Var "x", Num 1))))  (* Esperado: 2 : int *)

(* Teste de 'pipe' com função de duplicação *)
let _ = int_bse (Pipe (Num 2, Fn ("x", TyInt, Binop (Mult, Var "x", Num 2))))  (* Esperado: 4 : int *)

(* Teste de 'pipe' com erros de tipo *)
let _ = int_bse (Pipe (Num 2, Binop (Mult, Num 3, Num 2)))  (* Esperado: erro de tipo - pipe espera função *)
let _ = int_bse (Pipe (Bool false, Fn ("x", TyInt, Binop (Sum, Var "x", Num 1))))  (* Esperado: erro de tipo - tipos diferentes em pipe *)

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
(* Esperado: 30 *)
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
        List(Pair(Num 1, Num 10),
             List(Pair(Num 2, Num 20),
                  List(Pair(Num 3, Num 30),
                       List(Pair(Num 4, Num 40),
                            List(Pair(Num 5, Num 50), Nil(TyPair(TyInt, TyInt))))))),
        Let
          ("key",
           TyInt,
           Num 3,
           MatchWithNothing(App(App(Var "lookup", Var "key"), Var "base_dados"),
                            Num 0,
                            "n",
                            Var "n"))))
  
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
       List(Num 10,
            List(Num 50,
                 List(Num 30,
                      List(Num 40,
                           List(Num 50, Nil(TyInt)))))),
       MatchWithNothing(
         App(Var "list_max", Var "base_dados"),
         Num 0,
         "n",
         Var "n"
       )
      )
  )
    
let _ = int_bse(listMaxExpr)
