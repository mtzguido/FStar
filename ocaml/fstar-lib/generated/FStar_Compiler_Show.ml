open Prims
type 'a showable = {
  show: 'a -> Prims.string }
let __proj__Mkshowable__item__show : 'a . 'a showable -> 'a -> Prims.string =
  fun projectee -> match projectee with | { show;_} -> show
let show : 'a . 'a showable -> 'a -> Prims.string =
  fun projectee -> match projectee with | { show = show1;_} -> show1
let printableshow : 'a . 'a FStar_Class_Printable.printable -> 'a showable =
  fun uu___ -> { show = (fun x -> FStar_Class_Printable.to_string uu___ x) }