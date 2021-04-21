(*
    Copyright (c) 2020-21 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure Markdown: MARKDOWN =
struct

    (* This is a heavily cut-down version of Markdown. *)
    fun outputMarkdown(str, text) =
    let
        open TextIO
        val startText = String.explode text
        
        (* Does this line have a suitable termination followed
           by blank space or punctuation? *)
        local
            fun hasTermination (_, _, []) = NONE
            |   hasTermination (_, _, #"\n" :: _) = NONE
            |   hasTermination (match, count, hd :: tl) =
                if hd = match
                then
                (
                    case tl of
                        [] => SOME count
                    |   next :: _ =>
                        if Char.isSpace next orelse Char.isPunct next
                        then SOME count else NONE
                )
                else hasTermination (match, count+1, tl)
        in
            fun findTermination(mount, l) =
                hasTermination(mount, 0, l)
        end
        
        fun process(_, []) = ()

            (* Empty lines are paragraph separators.  Lines with just blank space
               are considered empty. *)
        |   process (_, #"\n" :: tl) =
            let
                fun skipBlank [] = NONE
                |   skipBlank (#"\n" :: tl) =
                    (
                        output(str, "</p>\n<p>");
                        SOME tl
                    )
                |   skipBlank (ch :: tl) =
                    if Char.isSpace ch
                    then skipBlank tl
                    else NONE
            in
                case skipBlank tl of
                    SOME newTl => process(true, newTl)
                |   NONE => (output1(str, #"\n"); process(true, tl))
            end

            (* Back quotes are code.  Treat as identifiers at the moment. *)
        |   process (_, #"`" :: tl) =
            let
                val () = output(str, "<identifier>")
                fun findEndQuote [] = []
                |   findEndQuote (#"`" :: tl) = tl
                |   findEndQuote (ch :: tl) = (output1(str, ch); findEndQuote tl)
                val continue = findEndQuote tl
                val () = output(str, "</identifier>");
            in
                process(false, continue)
            end
        
        (* Escape special HTML characters. *)
        |   process(_, #"&" :: tl) = (output(str, "&amp;"); process(false, tl))
        |   process(_, #"<" :: tl) = (output(str, "&lt;"); process(false, tl))
        |   process(_, #">" :: tl) = (output(str, "&gt;"); process(false, tl))
        
        |   process(true, #"*" :: tl) =
            (
                case findTermination(#"*", tl) of
                    NONE => (output1(str, #"*"); process(false, tl))
                |   SOME count =>
                    (
                        output(str, "<em>");
                        process(true, List.take(tl, count));
                        output(str, "</em>");
                        process(false, List.drop(tl, count+1))
                    )
            )

        |   process (_, ch :: tl) = (output1(str, ch); process(Char.isSpace ch, tl))
    in
        output(str, "<p>");
        process(true, startText);
        output(str, "</p>")
    end

end;

