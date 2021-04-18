(*
    Copyright (c) 2020 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)
(*
    Extracted from the Poly/ML lexical analyser
*)

functor LEX (
structure Symbols : SYMBOLS

) : LEX =

struct

  (* open Misc; *)

  open Symbols
  infix 8 eq neq;
  
  type lexan = 
    {
      stream:      unit -> char option,
      ch:          char ref,
      sy:          sys ref,
      id:          string ref,
      pushedSym:   sys ref,
      extraChars:  char list ref,
      text:        char list ref
    }
    
  (* The lexical analyser reads characters from the stream and updates the
     references in the lexan structure.  That's not perhaps very ML-like
     but the lexical analyser can be a hot-spot in the compiler unless it's
     made as fast as possible. *)

    val eofChar         = Char.chr 4; (* ctrl/D *)

    val isNumeric = Char.isDigit
    and isAlphabetic = Char.isAlpha
    and isWhiteSpace = Char.isSpace
    and isHexadecimal  = Char.isHexDigit

    (* For our purposes we include quote and underscore. *)
    fun isAlphaNumeric c = Char.isAlphaNum c orelse c = #"'" orelse c = #"_"

    val isOperator = Char.contains ":=<>+*!^/|&%~-?`@\\$#";

    (* The initial state looks like we've just processed a complete ML declaration *)
    fun initial stream : lexan =
        {
          stream      = stream,
          ch          = ref #" ",   (* " " - we've just "clobbered the ";" *)
          sy          = ref Semicolon,  (* ";"  *)
          id          = ref "",
          pushedSym   = ref Othersy,
          extraChars  = ref [],
          text        = ref []
        }

    fun nextCh({ch, stream, ...}) = ch := getOpt(stream(), eofChar)

    (* Skip over white space.  If we have to skip we record this as the END of
       the previous symbol.  If it turns out that the character is actually
       the start of a symbol then this will be set as the START by setSymbolStart. *)
    fun skipWhiteSpace (state as {ch = ref c, ...}:lexan) : char =
    if isWhiteSpace c
    then (nextCh state; skipWhiteSpace state)
    else c
 
    (* Leave string construction until we have all the characters.  Since
       Single character strings are the same as single characters it doesn't
       cost anything to apply "str" but it allows us to conatenate with any
       prefix string in one go. *)
    fun readChars (state as { ch, ... }) (isOk: char -> bool) (s: string) : string = 
    let
        fun loop (): string list =
        let
            val theChar  = ! ch;
        in
            if isOk theChar
            then (nextCh state; str theChar :: loop ())
            else []
        end;
    in
        concat (s :: loop ())
    end

    (* Read in a number. *)
    fun parseNumber (hasMinus, state as { sy, id, ch, extraChars, ... }) =
    (
        sy := IntegerConst;
        
        (* Copy digits into the buffer. *)
        id := readChars state isNumeric "";
        
        (* May be the end of an integer, part of a real number,
           or w for word or x for hex. *)
        (* Since "0" is a valid integer what follows it is only treated
           as part of the integer if it is well-formed.  If it is not
           we return the "0" as an integer constant and leave the rest
           to be returned.  In particular that means that 0wxz is
           the INTEGER constant "0" followed by the identifier "wxz".
           N.B. ~0w1 is ~0 w1 because word constants cannot begin with ~. *)
        if not hasMinus andalso !ch = #"w" andalso !id = "0"
        then (* word constant; if it's well formed. *)
        (
            nextCh state;
            if !ch = #"x"
            then
            (
                nextCh state;
                if isHexadecimal (!ch)
                then
                (
                    sy := WordConst;
                    id := readChars state isHexadecimal "0wx"
                )
                else (extraChars := [#"x", !ch]; ch := #"w")
            )
            else if isNumeric (!ch)
            then
            (
                sy := WordConst;
                id := readChars state isNumeric "0w"
            )
            else (extraChars := [!ch]; ch := #"w")
        )
        else if !ch = #"x" andalso !id = "0"
        then (* Hexadecimal integer constant. *)
        (
            nextCh state;
            if isHexadecimal (!ch)
            then id := readChars state isHexadecimal "0x"
            else (extraChars := [!ch]; ch := #"x")
        )
        else if !ch = #"." orelse
                !ch = #"E" orelse !ch = #"e" (* "e" is allowed in ML97 *)
        then (* possible real constant. *)
        (
            if !ch = #"."
            then
            (
               sy := RealConst;
               (* Add the "." to the string. *)
               id := !id ^ ".";
               nextCh state;
               (* Must be followed by at least one digit. *)
               if not (isNumeric (!ch))
               then raise Fail("malformed real number: " ^ !id ^ str(!ch))
               else id := readChars state isNumeric (!id)
            )
            else ();

            (* There's a nasty here.  We may actually have 1e~; which should
               (probably) be treated as 1 e ~ ; That means that if after we've
               read the e and possible ~ we find that the next character is not
               a digit we return the number read so far and leave the e, ~
               and whatever character we found to be read next time. *)
            if !ch = #"E" orelse !ch = #"e"
            then
            let
                val eChar = !ch
            in
                nextCh state;
               
                (* May be followed by a ~ *)
                (* If it's followed by a digit we have an exponent otherwise
                  we have a number followed by a identifier.  In that case
                  we have to leave the identifier until the next time we're called. *)
                if !ch = #"~"
                then
                (
                    nextCh state;
                    if isNumeric(!ch)
                    then (sy := RealConst; id := readChars state isNumeric (!id ^ "E~"))
                    else (extraChars := [#"~", !ch]; ch := eChar)
                )
                else
                (
                    if isNumeric(!ch)
                    then (sy := RealConst; id := readChars state isNumeric (!id ^ "E"))
                    else (extraChars := [!ch]; ch := eChar)
                )
            end
            else ()
        )
        else ()
     );

    fun parseString (state as { ch, id, ... }) =
    let
         (* The original version of this simply concatenated the characters onto "id".
            For very long strings that's expensive since each concatenation copies the
            existing string, resulting in quadratic performance.  This version creates a
            list and then implodes it.  DCJM 24/5/02. *)
        fun getString (soFar: char list) =
         (
            case !ch of
                #"\"" (* double-quote. *) => (* Finished - return result. *) (nextCh state; soFar)
    
            |   #"\n" => (nextCh state; raise Fail "end of line in string")
    
            |   #"\\" => (* Escape *)
                    let
                        val _ = nextCh state; (* Skip the escape char. *)
                        val next = !ch;   (* Look at the next char. *)
                        val _ = nextCh state;
                    in
                        (* Remove \f...\ sequences but otherwise leave the string
                           as it is.  Escape sequences are processed in the conversion
                           function.  In particular we can only decide whether \uxxxx
                           is valid when we know whether we are converting to Ascii or
                           Unicode. *)
                    if isWhiteSpace next
                    then
                        (
                        if skipWhiteSpace state = #"\\" then ()
                        else
                            (
                            raise Fail("unexpected character " ^
                               String.toString (str (!ch)) ^" in \\ ... \\");
                            while !ch <> #"\\"  andalso !ch <> #"\"" andalso !ch <> eofChar
                            do nextCh state
                            );
                        nextCh state;
                        getString soFar
                        )
                    else if next = #"^" (* \^c escape sequence for Control+c *)
                    then    let
                            val next2 = !ch;
                            val _ = nextCh state;
                        in  getString (next2 :: #"^" :: #"\\" :: soFar)
                        end
                    else getString (next :: #"\\" :: soFar)
                  end
    
            |   ch => (* Anything else *)
                    (
                     nextCh state;
                     if ch = eofChar then raise Fail "end of line in string"
                     else if Char.isPrint ch (* Ok if it's printable. *)
                     then getString (ch :: soFar)
                     else (* Report unprintable characters. *)
                        (
                        raise Fail("unprintable character " ^ Char.toString ch ^ " found in string");
                        getString soFar
                        )
                    )
         )

    in
        nextCh state; (* Skip the opening quote. *)

        id := String.implode(List.rev(getString []))
    end (* parseString *)

    (* parseComment deals with nested comments.
       Returns with !ch containing the first character AFTER the comment. *)
    fun parseComment ({ stream, ch, text, ... }: lexan) =
    let
        (* Extract the contents of the comment.  Returns the contents of the
           comment, ignoring any nested comments, and the character after the comment. *)
        fun skipCommentBody(body, firstCh) =
            (
                case (firstCh, stream()) of
                    (_, NONE) => raise Fail("end of file found in comment")

                |   (#"*", SOME #")") => (body, getOpt(stream (), eofChar)) (* End of comment - return next ch. *)

                |   (#"(", SOME #"*") => (* Nested comment *)
                    let
                        val first =
                            case stream() of
                                NONE => raise Fail "end of file found in comment"
                            |   SOME ch => ch
                        val (_, nextCh) = skipCommentBody([], first)
                    in
                        skipCommentBody (body, nextCh)
                    end

                |   (_, SOME char) => skipCommentBody(char :: body, char)
            )
        
        val firstChar =
            case stream() of
                NONE => raise Fail "end of file found in comment"
            |   SOME ch => ch
        val (commentCharsRev, nextCh) = skipCommentBody([], firstChar)

        (* Add this to the text if it begins with ! or starts and
           ends with * N.B.  The "text" is a reversed list of characters.
           commentCharsRev always starts with the final "*" of the comment. *)
        val _ =
            case (firstChar, commentCharsRev) of
                (#"!" , #"*" :: body) => text := body @ !text
            |   (#"*", #"*" :: #"*" :: body) => text := body @ !text
            |   _ => ()
    in 
        ch := nextCh
    end (* parseComment *);


    (* Sets "id" and "sy" if an identifier is read.
        Looks up a word to see if it is reserved.   *)
    fun parseIdent (state as { ch, id, sy, ... }) charsetTest first (* any characters read so far *) =
    let
        val idVal = readChars state charsetTest first;
    in      
    (* Qualified names may involve fields of different lexical form
       e.g. A.B.+ *)
        if !ch = #"." (* May be qualified *)
        then
        let
            val () = nextCh state;
            val c = !ch;
        in
             if isAlphabetic c
               then parseIdent state isAlphaNumeric (idVal ^ ".")
                 
             else if isOperator c
               then parseIdent state isOperator (idVal ^ ".")
                 
             else raise Fail("invalid identifier - "^ idVal ^ "." ^ str c)
        end
        else 
        (
            id := idVal;
            sy := (if 0 < size idVal andalso String.str(String.sub(idVal, 0)) = "'"
                   then TypeIdent
                   else lookup idVal)
        )
    end; (* parseIdent *)


    (* Main lexical analyser loop. *)
    fun parseToken (state as { ch, sy, ... }) =
    let
        val nextSym = skipWhiteSpace state (* remove leading spaces *)
    in

        case nextSym of
          #"~" => (* Either an operator or part of a number. *)
             (
               nextCh state;(* get next character *)
               if isNumeric (!ch)
               then parseNumber(true, state)
               else
                 (* Part of an operator. *) 
                 parseIdent state isOperator "~"
             )

        | #"#" =>(* Either an operator, which include a field selection or
                    a character constant.
                    N.B. It is not absolutely clear whether any separator
                    is allowed between # and the following string constant.
                    Assume that it isn't for the moment. *)
              (
                nextCh state;(* get next character *)
                if !ch = #"\""
                then (parseString state; sy := CharConst)
                else
                 (* Part of an operator. *) 
                 parseIdent state isOperator "#"
              )
        
        | #"\"" (* double quote. *) => (parseString state; sy := StringConst)
            
        | #";" =>
            (
                sy := Semicolon;
                (* This is a special case.  If this is the final semicolon
                   in the top-dec we mustn't read the next character because
                   that will be put into "ch" field of this lex object and will
                   then be discarded.  Instead we clobber this with a space so that
                   the normal space-skipping case will apply. *)
                ch := #" "
            )
            
        | #"," => (sy := Comma; nextCh state)
            
        | #"(" =>
              (
                nextCh state;
                if !ch <> #"*" then sy := LeftParen else parseComment state
              )
              
        | #")" => (sy := RightParen; nextCh state)
            
        | #"[" => (sy := LeftBrack; nextCh state)
            
        | #"]" => (sy := RightBrack; nextCh state)
            
        | #"_" => (sy := Underline; nextCh state)
            
        | #"{" => (sy := LeftCurly; nextCh state)
            
        | #"}" => (sy := RightCurly; nextCh state)

        | #"." => (* "..." *)
            (
                nextCh state;
                if !ch <> #"."
                then raise Fail("unknown symbol ." ^ str(!ch))
                else
                (
                    nextCh state;
                    if !ch <> #"." 
                    then raise Fail ("unknown symbol .." ^ str(!ch))
                    else (sy := ThreeDots; nextCh state)
                )
            )
              
         | firstCh =>
            (* These can't be so easily incorporated into a "case". *)
            if firstCh = eofChar
            then sy := AbortParse
          
            else if isNumeric firstCh
            then parseNumber(false, state)

            else if isAlphabetic firstCh orelse firstCh = #"'"
            then parseIdent state isAlphaNumeric ""
          
            else if isOperator firstCh
            (* excludes ~ which has already been done *)
            then parseIdent state isOperator ""
            
            else let (* illegal character *)
                val printableFirstCh = Char.toString firstCh
            in
                (* Report the character. *)
                raise Fail("unknown character \"" ^ printableFirstCh ^ "\"");
                nextCh state
            end;
        (* Read another token if this wasn't recognised. *)
        if (!sy = Othersy) then parseToken state else ()
    end; (* parseToken *)

    (* Insymbol - exported interface to lexical analyser. *)
    fun insymbol (state as {sy,pushedSym,...}:lexan) =
    if ! pushedSym <> Othersy then pushedSym := Othersy
    (* pushedSym is a hack to handle the difficulty of parsing
       val ('a, 'b) f = ... compared with val (a, b) = ... and the
       similar fun declarations. 
       It's also used to handle where type t = int and type ... compared
       with  where type t = int and S = sig ...*)
    else
    (
        if ! sy = AbortParse (* already end-of-file? *)
        then raise Fail("unexpected end of file encountered")
        else ();
      
        sy := Othersy; (* default - anything unrecognisable *)
      
        parseToken state
    ); (* insymbol *)

   (* exported version of sy and id. *)
   
   fun sy ({sy=ref sy, pushedSym = ref pushed, ...}:lexan) =
        if pushed <> Othersy then pushed else sy;

   fun id ({id=ref id,...}:lexan) = id
   
   fun getText({text as ref chs, ...}: lexan) = String.implode(List.rev chs) before text := []

    fun pushBackSymbol ({pushedSym,...}:lexan, sym) =
    (* TODO: This does not restore the location so parses such as val () = ... get the wrong
       location for the opening parenthesis. *)
        if !pushedSym <> Othersy then raise Fail "Attempt to push two parentheses"
        else pushedSym := sym

    structure Sharing =
    struct
        type lexan      = lexan
        and  sys        = sys
    end

end (* LEX functor body *);
