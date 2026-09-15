open Prims
type 'a funcs =
  | Pack of ('a -> 'a) * ('a -> 'a)
let uu___is_Pack (projectee : 'a funcs) : Prims.bool= true
let __proj__Pack__item__first (projectee : 'a funcs) : 'a -> 'a=
  match projectee with | Pack (first, second) -> first
let __proj__Pack__item__second (projectee : 'a funcs) : 'a -> 'a=
  match projectee with | Pack (first, second) -> second
let nested (f : 'a -> 'a) (x : 'a) : 'a= x
let stuck (p : 'a funcs) (x : 'a) : 'a=
  match p with | Pack (first, second) -> first x
let under_binder (f : 'a -> 'a) (x : 'a) : 'a= x
