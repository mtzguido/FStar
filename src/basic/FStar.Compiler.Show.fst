module FStar.Compiler.Show

open FStar.Compiler
open FStar.Compiler.Effect
open FStar.Class.Printable

class showable (a:Type) = {
  show : a -> ML string;
}

instance printableshow (_ : printable 'a) : Tot (showable 'a) = {
  show = (fun x -> to_string x);
}
