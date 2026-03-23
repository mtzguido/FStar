(** Merge adjacent HTML links with the same URL into a single <a> tag.

    Scans for the pattern:
      </a></span><span ...><a href="URL">
    and when the URL matches the currently open <a>, replaces it with:
      </span><span ...>
    so the <a> tag spans continuously across multiple <span>s. *)

let merge (html : string) : string =
  let len = String.length html in
  let buf = Buffer.create len in
  let close_reopen = "</a></span><span" in
  let close_reopen_len = String.length close_reopen in
  let a_open = "<a href=\"" in
  let a_open_len = String.length a_open in
  let last_href = ref "" in
  let i = ref 0 in
  while !i < len do
    (* Track <a href="..."> openings *)
    if !i + a_open_len < len
       && String.sub html !i a_open_len = a_open
    then begin
      let href_start = !i + a_open_len in
      let href_end = ref href_start in
      while !href_end < len && html.[!href_end] <> '"' do
        incr href_end
      done;
      last_href := String.sub html href_start (!href_end - href_start);
      (* Emit the whole <a href="..."> tag *)
      let tag_end = !href_end + 2 in (* skip the closing quote and > *)
      Buffer.add_string buf (String.sub html !i (tag_end - !i));
      i := tag_end
    end
    (* Check for </a></span><span...><a href="SAME"> pattern *)
    else if !i + close_reopen_len <= len
       && String.sub html !i close_reopen_len = close_reopen
    then begin
      (* Find end of the <span ...> tag *)
      let span_end = ref (!i + close_reopen_len) in
      while !span_end < len && html.[!span_end] <> '>' do
        incr span_end
      done;
      let span_close = !span_end + 1 in
      (* Check if next is <a href="SAME_URL"> *)
      if span_close + a_open_len < len
         && String.sub html span_close a_open_len = a_open
      then begin
        let href_start = span_close + a_open_len in
        let href_end = ref href_start in
        while !href_end < len && html.[!href_end] <> '"' do
          incr href_end
        done;
        let next_href = String.sub html href_start (!href_end - href_start) in
        if !last_href <> "" && !last_href = next_href then begin
          (* Merge: skip </a> at start and <a href="..."> at end *)
          (* Emit just </span><span ...> *)
          Buffer.add_string buf (String.sub html (!i + 4) (span_close - !i - 4));
          i := !href_end + 2
        end else begin
          Buffer.add_char buf html.[!i];
          incr i
        end
      end else begin
        Buffer.add_char buf html.[!i];
        incr i
      end
    end
    (* Track </a> to clear last_href *)
    else if !i + 4 <= len && String.sub html !i 4 = "</a>" then begin
      last_href := "";
      Buffer.add_string buf "</a>";
      i := !i + 4
    end
    else begin
      Buffer.add_char buf html.[!i];
      incr i
    end
  done;
  Buffer.contents buf
