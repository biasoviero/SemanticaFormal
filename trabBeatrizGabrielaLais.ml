(*++++++++++++++++++++++++++++++++++++++*)
(*  Interpretador para L1               *)
(*   - inferência de tipos              *)
(*   - avaliador big step com ambiente  *)
(*++++++++++++++++++++++++++++++++++++++*)



(**+++++++++++++++++++++++++++++++++++++++++*)
(*  SINTAXE, AMBIENTE de TIPOS e de VALORES *)
(*++++++++++++++++++++++++++++++++++++++++++*)

type tipo =
    TyInt
  | TyBool
  | TyFn of tipo * tipo
  | TyPair of tipo * tipo
  | TyMaybe of tipo
  | TyList of tipo
              

type ident = string

type op = Sum | Sub | Mult | Div | Eq | Gt | Lt | Geq | Leq 

type expr =
  | Num of int
  | Var of ident
  | Bool of bool
  | Binop of op * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | If of expr * expr * expr
  | Fn of ident * tipo * expr
  | App of expr * expr
  | Let of ident * tipo * expr * expr
  | LetRec of ident * tipo * expr  * expr
  | Nothing of tipo
  | Just of expr
  | MatchWithNothing of expr * expr * ident * expr
  | Nil of tipo
  | List of expr * expr
  | MatchWithNil of expr * expr * ident * ident * expr
  | Pipe of expr * expr
  
              
  
type valor = 
    VNum of int
  | VBool of bool
  | VPair of valor * valor
  | VClos  of ident * expr * renv
  | VRClos of ident * ident * expr * renv 
  | VNothing of tipo 
  | VJust of valor
  | VNil of tipo
  | VList of valor * valor
and  
  renv = (ident * valor) list
              
type tenv = (ident * tipo) list

  
(* exceções que não devem ocorrer  *)

exception BugParser
  

(**+++++++++++++++++++++++++++++++++++++++++*)
(*         INFERÊNCIA DE TIPOS              *)
(*++++++++++++++++++++++++++++++++++++++++++*)


exception TypeError of string


let rec typeinfer (tenv:tenv) (e:expr) : tipo =
  match e with

    (* TInt  *)
  | Num _ -> TyInt

    (* TVar *)
  | Var x ->
      (match List.assoc_opt x tenv with
         Some t -> t
       | None -> raise (TypeError ("variavel nao declarada:" ^ x)))

    (* TBool *)
  | Bool _ -> TyBool 
  

    (*TOP+  e outras para demais peradores binários *)
  | Binop(oper,e1,e2) ->
      let t1 = typeinfer tenv e1 in
      let t2 = typeinfer tenv e2 in
      if t1 = TyInt && t2 = TyInt then
        (match oper with
           Sum | Sub | Mult |Div -> TyInt
         | Eq | Lt | Gt | Geq | Leq -> TyBool)
      else raise (TypeError "operando nao é do tipo int")

    (* TPair *)
  | Pair(e1,e2) -> TyPair(typeinfer tenv e1, typeinfer tenv e2)
  (* TFst *)
  | Fst e1 ->
      (match typeinfer tenv e1 with
         TyPair(t1,_) -> t1
       | _ -> raise (TypeError "fst espera tipo par"))
    (* TSnd  *)
  | Snd e1 ->
      (match typeinfer tenv e1 with
         TyPair(_,t2) -> t2
       | _ -> raise (TypeError "snd espera tipo par"))

    (* TIf  *)
  | If(e1,e2,e3) ->
      (match typeinfer tenv e1 with
         TyBool ->
           let t2 = typeinfer tenv e2 in
           let t3 = typeinfer tenv e3
           in if t2 = t3 then t2
           else raise (TypeError "then/else com tipos diferentes")
       | _ -> raise (TypeError "condição de IF não é do tipo bool"))

    (* TFn *)
  | Fn(x,t,e1) ->
      let t1 = typeinfer ((x,t) :: tenv) e1
      in TyFn(t,t1)

    (* TApp *)
  | App(e1,e2) ->
      (match typeinfer tenv e1 with
         TyFn(t, t') ->  if (typeinfer tenv e2) = t then t'
           else raise (TypeError "tipo argumento errado" )
       | _ -> raise (TypeError "tipo função era esperado"))

    (* TLet *)
  | Let(x,t,e1,e2) ->
      if (typeinfer tenv e1) = t then typeinfer ((x,t) :: tenv) e2
      else raise (TypeError "expressão nao é do tipo declarado em Let" )

    (* TLetRec *)
  | LetRec(f,(TyFn(t1,t2) as tf), Fn(x,tx,e1), e2) ->
      let tenv_com_tf = (f,tf) :: tenv in
      let tenv_com_tf_tx = (x,tx) :: tenv_com_tf in
      if (typeinfer tenv_com_tf_tx e1) = t2
      then typeinfer tenv_com_tf e2
      else raise (TypeError "tipo da funcao recursiva é diferente do declarado")
  | LetRec _ -> raise BugParser

    (* TNothing *)
  | Nothing t -> TyMaybe t

    (* TJust *)
  | Just e -> TyMaybe (typeinfer tenv e)

    (* TMatchWithNothing *)
  | MatchWithNothing(e,e1,x,e2) ->
      (match typeinfer tenv e with
         TyMaybe t' ->
           let t1 = typeinfer tenv e1 in
           let t2 = typeinfer ((x,t') :: tenv) e2
           in if t1 = t2 then t1
           else raise (TypeError "tipos diferentes em match")
       | _ -> raise (TypeError "match espera tipo maybe"))

    (* TNil *)
  | Nil t -> TyList t

    (* TList *)
  | List(e1,e2) ->
      let t1 = typeinfer tenv e1 in
      let t2 = typeinfer tenv e2 in
      (match t2 with
         TyList t' when t1 = t' -> TyList t1
       | _ -> raise (TypeError "tipos diferentes em lista"))
          
  (* TMatchWithNil *)
  | MatchWithNil(e,e1,x,xs,e2) ->
      (match typeinfer tenv e with
         TyList t' ->
           let t1 = typeinfer tenv e1 in
           let t2 = typeinfer ((x,t') :: (xs,TyList t') :: tenv) e2
           in if t1 = t2 then t1
           else raise (TypeError "tipos diferentes em match")
       | _ -> raise (TypeError "match espera tipo lista"))

  (* TPipe *)
  | Pipe(e1,e2) ->
      (match typeinfer tenv e2 with
         TyFn(t1,t2) ->
           let t = typeinfer tenv e1
           in if t = t1 then t2
           else raise (TypeError "tipos diferentes em pipe")
       | _ -> raise (TypeError "pipe espera função"))
                  
  
(**+++++++++++++++++++++++++++++++++++++++++*)
(*                 AVALIADOR                *)
(*++++++++++++++++++++++++++++++++++++++++++*)


exception BugTypeInfer

let compute (oper: op) (v1: valor) (v2: valor) : valor =
  match (oper, v1, v2) with
    (Sum, VNum(n1), VNum(n2)) -> VNum (n1 + n2)
  | (Sub, VNum(n1), VNum(n2)) -> VNum (n1 - n2)
  | (Mult, VNum(n1),VNum(n2)) -> VNum (n1 * n2) 
  | (Div, VNum(n1),VNum(n2))  -> VNum (n1 / n2)    
  | (Eq, VNum(n1), VNum(n2))  -> VBool (n1 = n2) 
  | (Gt, VNum(n1), VNum(n2))  -> VBool (n1 > n2)  
  | (Lt, VNum(n1), VNum(n2))  -> VBool (n1 < n2)  
  | (Geq, VNum(n1), VNum(n2)) -> VBool (n1 >= n2) 
  | (Leq, VNum(n1), VNum(n2)) -> VBool (n1 <= n2) 
  | _ -> raise BugTypeInfer


let rec eval (renv:renv) (e:expr) :valor =
  match e with
    Num n -> VNum n
  
  | Var x ->
      (match List.assoc_opt x renv with
         Some v -> v
       | None -> raise BugTypeInfer ) 
      
  | Bool b -> VBool b 
    
  | Binop(oper,e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2 in
      compute oper v1 v2
        
  | Pair(e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2
      in VPair(v1,v2)

  | Fst e ->
      (match eval renv e with
       | VPair(v1,_) -> v1
       | _ -> raise BugTypeInfer)

  | Snd e ->
      (match eval renv e with
       | VPair(_,v2) -> v2
       | _ -> raise BugTypeInfer)


  | If(e1,e2,e3) ->
      (match eval renv e1 with
         VBool true  -> eval renv e2
       | VBool false -> eval renv e3
       | _ -> raise BugTypeInfer )
      
  | Fn(x,_,e1)  -> VClos(x,e1, renv)
                     
  | App(e1,e2) ->
      let v1 = eval renv e1 in
      let v2 = eval renv e2 in
      (match v1 with 
         VClos(   x,e',renv') ->
           eval  (         (x,v2) :: renv')  e' 
       | VRClos(f,x,e',renv') -> 
           eval  ((f,v1) ::(x,v2) :: renv')  e' 
       | _  -> raise BugTypeInfer) 

  | Let(x,_,e1,e2) ->
      let v1 = eval renv e1
      in eval ((x,v1) :: renv) e2

  | LetRec(f,TyFn(t1,t2),Fn(x,tx,e1), e2) when t1 = tx ->
      let renv'=  (f, VRClos(f,x,e1,renv)) :: renv
      in eval renv' e2

  | Nothing t -> VNothing t

  | Just e -> 
      let v = eval renv e 
      in VJust v
        
  | MatchWithNothing(e,e1,x,e2) ->
      let v' = eval renv e in
      (match v' with
         VNothing _ -> eval renv e1
       | VJust v -> eval ((x,v) :: renv) e2
       | _ -> raise BugTypeInfer)
    
  | Nil t -> VNil t

  | List(e, e1) ->
      let vh = eval renv e in
      let vt = eval renv e1
      in VList(vh,vt)
  
  | MatchWithNil(e,e1,x,xs,e2) ->
      let v' = eval renv e in
      (match v' with
         VNil _ -> eval renv e1
       | VList(vh,vt) -> eval ((x,vh) :: (xs,vt) :: renv) e2
       | _ -> raise BugTypeInfer)

  | Pipe(e1, e2) ->
      let v' = eval renv e1 in
      let v =  eval renv e2 in
      (match v with
         VClos(x, e, renv') ->
           eval ((x, v') :: renv') e
       | VRClos(f, x, e, renv') ->
           eval ((f, v) :: (x, v') :: renv') e
       | _ -> raise BugTypeInfer)
        
  | LetRec _ -> raise BugParser 
                  
                  
(* função auxiliar que converte tipo para string *)

let rec ttos (t:tipo) : string =
  match t with
    TyInt  -> "int"
  | TyBool -> "bool"
  | TyFn(t1,t2)   ->  "("  ^ (ttos t1) ^ " --> " ^ (ttos t2) ^ ")"
  | TyPair(t1,t2) ->  "("  ^ (ttos t1) ^ " * "   ^ (ttos t2) ^ ")"
  | TyMaybe t -> "maybe " ^ (ttos t)
  | TyList t -> "list " ^ (ttos t)

(* função auxiliar que converte valor para string *)

let rec vtos (v: valor) : string =
  match v with
    VNum n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VPair(v1, v2) ->
      "(" ^ vtos v1 ^ "," ^ vtos v2 ^ ")"
  | VClos _ ->  "fn"
  | VRClos _ -> "fn"
  | VNothing _ -> "nothing"
  | VJust v -> "just " ^ vtos v
  | VNil _ -> "nil"
  | VList(vh, vt) -> vtos vh ^ " :: " ^ vtos vt
  

(* principal do interpretador *)

let int_bse (e:expr) : unit =
  try
    let t = typeinfer [] e in
    let v = eval [] e
    in  print_string ((vtos v) ^ " : " ^ (ttos t))
  with
    TypeError msg ->  print_string ("erro de tipo - " ^ msg) 
  | BugTypeInfer  ->  print_string "corrigir bug em typeinfer"
  | BugParser     ->  print_string "corrigir bug no parser para let rec"