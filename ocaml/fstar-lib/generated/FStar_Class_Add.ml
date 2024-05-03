open Prims
type 'a additive = {
  zero: 'a ;
  plus: 'a -> 'a -> 'a }
let __proj__Mkadditive__item__zero : 'a . 'a additive -> 'a =
  fun x4 -> match x4 with | { zero = azero; plus = aplus;_} -> azero
let zero : 'a . 'a additive -> 'a =
  fun x4 -> match x4 with | { zero = azero; plus = aplus;_} -> azero
let __proj__Mkadditive__item__plus : 'a . 'a additive -> 'a -> 'a -> 'a =
  fun x5 -> match x5 with | { zero = azero; plus = aplus;_} -> aplus
let plus : 'a . 'a additive -> 'a -> 'a -> 'a =
  fun x5 -> match x5 with | { zero = azero; plus = aplus;_} -> aplus
let op_Plus_Plus : 'a . 'a additive -> 'a -> 'a -> 'a = plus
let (add_int : Prims.int additive) = { zero = Prims.int_zero; plus = (+) }
let (add_bool : Prims.bool additive) = { zero = false; plus = (||) }
let add_list : 'a . unit -> 'a Prims.list additive =
  fun uu___ -> { zero = []; plus = FStar_List_Tot_Base.append }