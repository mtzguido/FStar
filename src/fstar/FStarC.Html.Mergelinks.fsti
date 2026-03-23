module FStarC.Html.Mergelinks

open FStarC.Effect

(** Merge adjacent HTML links with the same URL into a single <a> tag.
    Replaces </a></span><span ...><a href="URL"> with </span><span ...>
    when the URL matches, so the underline is continuous. *)
val merge : string -> string
