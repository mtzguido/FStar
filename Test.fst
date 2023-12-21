module Test

module US = FStar.SizeT

let x : US.t = normalize_term (US.uint_to_t (normalize_term (US.v 4sz)))
