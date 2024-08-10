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

