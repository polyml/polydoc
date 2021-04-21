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
(*
    Extracted from the Poly/ML parser
*)
functor PARSETREE (
    structure Markdown : MARKDOWN
) : PARSETREE =
struct
    open Markdown

    datatype typeParseTree =
        UnitTree
    |   Labelled of (string * typeParseTree) list
    |   TypeVariable of string
    |   TypeConstruction of string * typeParseTree list
    |   TypeProduct of typeParseTree list
    |   TypeFunction of typeParseTree * typeParseTree
    
    datatype specParseTree =
        StructSpec of (string * sigNature) list * string
    |   ValSpec of (string * typeParseTree) list * string
    |   ExSpec of (string * typeParseTree option) list * string
    |   DatatypeSpec of (string * string list * (string * typeParseTree option) list) list * string
    |   DatatypeReplication of {newType:string, oldType: string}
    |   TypeSpec of { items: (string * string list * typeParseTree option) list, typeKind: typeKind, text: string }
    |   IncludeSig of sigNature list
    |   Sharing of { isType: bool, shares: string list }
    
    and sigNature =
        NamedSig of string
    |   SigEnd of specParseTree list
    |   SigWhere of sigNature * typeParseTree * typeParseTree
    
    and typeKind = TypeKindType | TypeKindEqType | TypeKindWithType

    and functorArg =
        NoFunctorArg
    |   AnonFunctorArg of specParseTree list
    |   NamedFunctorArg of string * sigNature
    
    datatype program =
        Signature of (string * sigNature) list
    |   Structure of (string * sigNature option) list
    |   Functor of (string * sigNature option * functorArg) list

    open PolyML
    
    local
        fun escape #">" = "&gt;"
        |   escape #"<" = "&lt;"
        |   escape #"&" = "&amp;"
        |   escape c    = str c
    in
        val htmlEscape = String.translate escape
        fun prettyEscape str =
            PrettyStringWithWidth(String.translate escape str, String.size str)
    end
    
    fun printList _       ([], _) = []
    |   printList doPrint ([v], _) = [doPrint v]
    |   printList doPrint (v ::vs, separator) =
            PrettyBlock (0, false, [],
                [
                    doPrint v,
                    PrettyBreak
                       (if separator = "," orelse separator = ";" orelse separator = "" then 0 else 1, 0),
                    PrettyString separator
                ]
                ) ::
            PrettyBreak (1, 0) ::
            printList doPrint (vs, separator)

    fun prettyType t =
    let
        (* prints a block of items *)
        fun parenthesise t =
            PrettyBlock (0, false, [],
                [ PrettyString "(", prettyType t, PrettyString ")" ]);

        (* prints a sequence of items *)
        fun prettyList [] _: pretty list = []

        |   prettyList [v] separator =
                if separator = "*" andalso
                    (case v of TypeProduct _ => true | TypeFunction _ => true | _ => false)
                then (* Must bracket the expression *) [parenthesise v]
                else [prettyType v]

        |   prettyList (v :: T) separator =
                PrettyBlock (0, false, [],
                    [if separator = "*" andalso (case v of TypeProduct _ => true | TypeFunction _ => true | _ => false)
                    then (* Must bracket the expression *) parenthesise v
                    else prettyType v,
                    PrettyBreak (if separator = "," then 0 else 1, 0),
                    PrettyString separator
                    ]) ::
                PrettyBreak (1, 0) ::
                prettyList T  separator
    in 
        case t of
            UnitTree => PrettyString "()"

        |   TypeVariable tyVar => prettyEscape tyVar
  
            (* Type construction. *)
        |   TypeConstruction(constrName, args) =>
            (
                case args of
                    [] => prettyEscape constrName
                |   args as argVal :: tl =>
                        PrettyBlock (0, false, [],
                        [
                            (* If we have just a single argument and it's just a type constructor
                               or a construction we don't need to parenthesise it. *)
                            if null tl andalso (case argVal of TypeProduct _ => false | TypeFunction _ => false | _ => true)
                            then prettyType argVal
                            else PrettyBlock(0, false, [],
                                     [PrettyString "(", PrettyBreak (0, 0)]
                                     @ prettyList args ","
                                     @ [PrettyBreak (0, 0), PrettyString ")"]
                                 ),
                            PrettyBreak(1, 0),
                            prettyEscape constrName
                        ])
            )
    
        |   TypeFunction (arg, result) =>
                PrettyBlock (0, false, [],
                    [
                    (* If the argument is a function it must be printed as (a-> b)->.. *)
                    case arg of TypeFunction _ => parenthesise arg | _ => prettyType arg,
                    PrettyBreak(1, 2),
                    prettyEscape "->",
                    PrettyBreak (1, 2),
                    prettyType result
                    ])

        |   TypeProduct fields => PrettyBlock (0, false, [], prettyList fields "*")

        |   Labelled recList =>
            let
                fun pRec [] = []
                |   pRec [(name, typeof)] =
                        [PrettyBlock(0, false, [],
                            [prettyEscape (name ^ ":"), prettyType typeof])]
                |   pRec ((name, typeof) :: T) =
                        PrettyBlock(0, false, [],
                            [prettyEscape (name ^ ":"), prettyType typeof, PrettyBreak (0, 0), PrettyString ","]) ::
                                PrettyBreak (1, 0) :: pRec T
            in
                PrettyBlock (2, false, [],
                    PrettyString "{" :: PrettyBreak(0, 4) :: pRec recList @ [ PrettyBreak(0, 4), PrettyString "}"]
                    )
            end
    end
    
    (* Multiple type variables have to be parenthesised. *)
    and prettyTypeVars [] = []
    |   prettyTypeVars [single] = [PrettyString single]
    |   prettyTypeVars args =
            [PrettyBlock (0, false, [],
                [PrettyString "(", PrettyBreak (0, 0)] @ printList PrettyString(args, ",") @ [PrettyBreak (0, 0), PrettyString ")"])]
    
    fun prettyItems _ [] = []
    |   prettyItems (pretty, _) [single] = [pretty single]
    |   prettyItems (pretty, indent) (hd::tl) = pretty hd :: PrettyBreak(1, indent) :: prettyItems(pretty, indent) tl

    (* A series of items separated by "and". *)
    fun prettyAnds _ [] = []
    |   prettyAnds(doItem, first) [single] = [doItem(single, first)]
    |   prettyAnds(doItem, first) (hd :: tl) =
            doItem(hd, first) :: PrettyBreak(1, 0) ::prettyAnds(doItem, "and") tl

    fun nameAndLink false name = prettyEscape name
    |   nameAndLink true name =
        let
            val nameWithLinks =
                concat["<a name=\"", htmlEscape name, "\" id=\"", htmlEscape name, "\"></a>", name]
        in
            PrettyStringWithWidth(nameWithLinks, String.size name)
        end
    
    (** Pretty print the specification.  decorate is true if the name should be set as an anchor. **)
    fun prettySpec decorate (StructSpec(items, _)) =
        let
            fun nameAndSig((name, signat), pref) =
                PrettyBlock(0, true, [],
                    [PrettyBlock(0, false, [],
                        [PrettyString pref, PrettyBreak(1, 0), nameAndLink decorate name,
                         PrettyBreak(0,0), PrettyString ":"]),
                     PrettyBreak(1, 0), prettySig signat])
        in
            PrettyBlock(0, true, [], prettyAnds(nameAndSig, "structure") items)
        end

    |   prettySpec decorate (ValSpec(items, _)) =
        let
            fun nameAndType ((name, typ), pref) =
                PrettyBlock(0, true, [], [
                    PrettyBlock(0, false, [],
                        [PrettyString pref, PrettyBreak(1, 0), nameAndLink decorate name, PrettyBreak(0,0), PrettyString ":"]),
                        PrettyBreak(1, 4), prettyType typ])
        in
            PrettyBlock(0, true, [], prettyAnds(nameAndType, "val") items)
        end

    |   prettySpec decorate(ExSpec(items, _)) =
        let
            fun nameAndType ((name, SOME typ), pref) =
                PrettyBlock(0, true, [], [
                    PrettyBlock(0, false, [],
                        [PrettyString pref, PrettyBreak(1, 0), nameAndLink decorate name, PrettyBreak(0,0), PrettyString ":"]),
                        PrettyBreak(1, 4), prettyType typ])
            |   nameAndType ((name, NONE), pref) =
                    PrettyBlock(0, false, [], [PrettyString pref, PrettyBreak(1, 0), nameAndLink decorate name])
        in
            PrettyBlock(0, true, [], prettyAnds(nameAndType, "exception") items)
        end
    
    |   prettySpec decorate (DatatypeSpec(items, _)) =
        let
            fun printConstructor (constrName, constrArg) =
                PrettyBlock (2, false, [],
                    prettyEscape constrName ::
                    (
                        case constrArg of
                            NONE => []
                        |   SOME argType =>
                            [
                                PrettyBreak (1, 0),
                                PrettyString "of",
                                PrettyBreak (1, 0),
                                prettyType argType
                            ]
                    )
                )

            fun prettyDatatype((name, args, constrs), pref) =
                PrettyBlock(4, true, [],
                    [PrettyBlock(0, false, [],
                        [PrettyString pref, PrettyBreak(1, 0)] @ prettyTypeVars args @
                            [nameAndLink decorate name, PrettyBreak(1, 4), PrettyString "="]),
                        PrettyBreak (1, 0)] @
                        printList printConstructor (constrs, "|")
                )
        in
            PrettyBlock(0, true, [], prettyAnds(prettyDatatype, "datatype") items)
        end

    |   prettySpec decorate (DatatypeReplication {newType, oldType}) =
            PrettyBlock (3, true, [],
                [
                    PrettyString "datatype",
                    PrettyBreak (1, 0),
                    nameAndLink decorate newType,
                    PrettyBreak (1, 0),
                    PrettyString "=",
                    PrettyBreak (1, 0),
                    PrettyString "datatype",
                    PrettyBreak (1, 0),
                    prettyEscape oldType
                ]
            )
    
    |   prettySpec decorate (TypeSpec{ items, typeKind, ... }) =
        let
            fun nameAndType ((name, args, NONE), pref) =
                PrettyBlock(0, true, [],
                    [PrettyString pref, PrettyBreak(1, 0)] @ prettyTypeVars args @
                        [nameAndLink decorate name, PrettyBreak(1, 4)])

            |   nameAndType ((name, args, SOME typ), pref) =
                PrettyBlock(0, true, [],
                    [PrettyBlock(0, false, [],
                        [PrettyString pref, PrettyBreak(1, 0)] @ prettyTypeVars args @
                        [nameAndLink decorate name, PrettyBreak(1, 0), PrettyString "="]),
                     PrettyBreak(1, 4), prettyType typ])
            val kind =
                case typeKind of TypeKindType => "type" | TypeKindEqType => "eqtype" | TypeKindWithType => "withtype"
        in
            PrettyBlock(0, true, [], prettyAnds(nameAndType, kind) items)
        end

    |   prettySpec _ (Sharing { isType, shares, ... }) =
        PrettyBlock (4, false, [],
            PrettyString "sharing" ::
            PrettyBreak (1, 0) ::
            (
                if not isType then []
                else [ PrettyString "type", PrettyBreak (1, 0) ]
            ) @
            printList PrettyString (shares, "=")
        )

    |   prettySpec _ (IncludeSig sigList) =
        PrettyBlock (4, true, [],
            PrettyString "include" ::
            PrettyBreak (1, 0) ::
            printList prettySig (sigList, "")
        )

    
    and prettySig(NamedSig name) = PrettyString name
    |   prettySig(SigEnd specs) =
            PrettyBlock(0, true, [],
                PrettyString "sig" ::
                    PrettyBreak(1, 4) :: prettyItems (prettySpec false, 4) specs @
                        [PrettyBreak(1, 0), PrettyString "end"])
    |   prettySig(SigWhere(signat, srcType, typ)) =
            PrettyBlock(0, false, [],
                [prettySig signat, PrettyBreak(1, 4),
                    PrettyBlock(0, false, [],
                        [PrettyString "where", PrettyBreak(1, 0), PrettyString "type", PrettyBreak(1, 0), 
                            prettyType srcType, PrettyBreak(1, 0), PrettyString "=",
                            PrettyBreak(1, 0), prettyType typ])])
                            
    
    local
        fun nameAndSig((name, signat), pref) =
                PrettyBlock(0, true, [], [PrettyString(pref ^ " " ^ name ^ ":"), PrettyBreak(1, 0), prettySig signat])
        fun nameAndOptSig((name, SOME signat), pref) =
                PrettyBlock(0, true, [], [PrettyString(pref ^ " " ^ name ^ ":"), PrettyBreak(1, 0), prettySig signat])
        |   nameAndOptSig((name, NONE), pref) = PrettyString(pref ^ " " ^ name)

        fun functorArg NoFunctorArg = PrettyString "()"
        |   functorArg (AnonFunctorArg specs) =
                PrettyBlock(0, true, [],
                PrettyString "(" ::
                    PrettyBreak(1, 4) :: prettyItems (prettySpec false, 4) specs @
                        [PrettyBreak(1, 0), PrettyString ")"])
        |   functorArg(NamedFunctorArg(name, signat)) =
                PrettyBlock(0, true, [],
                    [PrettyString("(" ^ name ^ ":"), PrettyBreak(1, 0), prettySig signat, PrettyBreak(0, 0), PrettyString ")"])

        fun functorEntry((name, NONE, arg), pref) =
                PrettyBlock(0, false, [], [PrettyString(pref ^ " " ^ name), PrettyBreak(0, 0), functorArg arg])
        |   functorEntry((name, SOME signat, arg), pref) =
                PrettyBlock(0, false, [],
                    [PrettyString(pref ^ " " ^ name), PrettyBreak(0, 0), functorArg arg,
                     PrettyBreak(0, 0), PrettyString ":", PrettyBreak(1, 0), prettySig signat])
    in
        fun prettyTopDec(Signature items) =
            PrettyBlock(0, true, [], prettyAnds(nameAndSig, "signature") items)
        |   prettyTopDec(Structure items) =
            PrettyBlock(0, true, [], prettyAnds(nameAndOptSig, "structure") items)
        |   prettyTopDec(Functor items) =
            PrettyBlock(0, true, [], prettyAnds(functorEntry, "functor") items)
    end
    
    fun prettyProgram items = PrettyBlock(0, true, [], prettyItems(prettyTopDec, 0) items)

    local
        fun detailSpec str (StructSpec(items, text)) =
            let
                (* Substructure - just put in "structure <name>".  Don't repeat the
                   signature but follow it by the details of the entries in the signature. *)
                fun nameAndSig((name, _), pref) =
                let
                    val escName = htmlEscape name
                    val nameWithLinks =
                        concat[pref, " <a name=\"", escName, "\" id=\"", escName, "\"></a>", escName]
                    val realSize = String.size(concat[pref, " ", name])
                in
                    PrettyStringWithWidth(nameWithLinks, realSize)
                end

                val pretty = PrettyBlock(0, true, [], prettyAnds(nameAndSig, "structure") items)
                open TextIO
            in
                output(str, "<binding>\n<code>");
                PolyML.prettyPrint(fn s => output(str, s), 70) pretty;
                output(str, "</code>\n<text>");
                outputMarkdown(str, text);
                output(str, "</text>\n</binding>\n");
                List.app (fn (_, sign) => detailSig str sign) items
            end

        |   detailSpec str (ValSpec(items, text)) =
            let
                fun nameAndType ((name, typ), pref) =
                let
                    val escName = htmlEscape name
                    val nameWithLinks =
                        concat[pref, " <a name=\"", escName, "\" id=\"", escName, "\"></a>", escName, ":"]
                    (* Apparently HTML5 ids can be any character so we don't need to escape. *)
                    val realSize = String.size(concat[pref, " ", name, ":"])
                in
                    PrettyBlock(0, true, [],
                        [PrettyStringWithWidth(nameWithLinks, realSize), PrettyBreak(1, 4), prettyType typ])
                end

                open TextIO
                val pretty =
                    PrettyBlock(0, true, [], prettyAnds(nameAndType, "val") items)
            in
                output(str, "<binding>\n<code>");
                PolyML.prettyPrint(fn s => output(str, s), 70) pretty;
                output(str, "</code>\n<text>");
                outputMarkdown(str, text);
                output(str, "</text>\n</binding>\n")
            end

        |   detailSpec str (spec as ExSpec(_, text)) =
            let
                open TextIO
                val pretty = prettySpec true spec
            in
                output(str, "<binding>\n<code>");
                PolyML.prettyPrint(fn s => output(str, s), 70) pretty;
                output(str, "</code>\n<text>");
                outputMarkdown(str, text);
                output(str, "</text>\n</binding>\n") 
            end

        |   detailSpec str (spec as DatatypeSpec(_, text)) =
            let
                val pretty = prettySpec true spec
                open TextIO
            in
                output(str, "<binding>\n<code>");
                PolyML.prettyPrint(fn s => output(str, s), 70) pretty;
                output(str, "</code>\n<text>");
                outputMarkdown(str, text);
                output(str, "</text>\n</binding>\n")
            end

        |   detailSpec str (spec as DatatypeReplication _) =
            let
                val pretty = prettySpec true spec
                open TextIO
            in
                output(str, "<binding>\n<code>");
                PolyML.prettyPrint(fn s => output(str, s), 70) pretty;
                output(str, "</code>");
                (* TODO: This doesn't have any associated text. *)
                (*output(str, "<text>");
                outputMarkdown(str, text);
                output(str, "</text>"); *)
                output(str, "</binding>\n") 
            end

        |   detailSpec str (spec as TypeSpec{ text, ... }) =
            let
                open TextIO
                val pretty = prettySpec true spec
            in
                output(str, "<binding>\n<code>");
                PolyML.prettyPrint(fn s => output(str, s), 70) pretty;
                output(str, "</code>\n<text>");
                outputMarkdown(str, text);
                output(str, "</text>\n</binding>\n") 
            end
        
            (* These aren't needed in the detail *)
        |   detailSpec _ (IncludeSig _) = ()
        |   detailSpec _ (Sharing _) = ()

        and detailSig str (SigEnd sl) = List.app (detailSpec str) sl
        |   detailSig _ (NamedSig _) = ()
        |   detailSig str (SigWhere(sign, _, _)) = detailSig str sign
        

        fun detailProgram str (Signature items) =
            List.app (fn (_, sign) => detailSig str sign) items

        |   detailProgram str (Structure items) =
            List.app (fn (_, SOME sign) => detailSig str sign | _ => ()) items

        |   detailProgram str (Functor items) =
            List.app (fn (_, SOME sign, _) => detailSig str sign | _ => ()) items
    in
        fun outputProgram (program, stream) =
        let
            open TextIO
            val pretty = prettyProgram program
        in
            output(stream, "<summary>");
            PolyML.prettyPrint(fn s => output(stream, s), 70) pretty;
            output(stream, "</summary>\n");
 
            output(stream, "<bindings>");
            List.app (detailProgram stream) program;
            output(stream, "</bindings>\n")
        end
    end

    structure Sharing =
    struct
        type typeParseTree = typeParseTree
        and  specParseTree = specParseTree
        and  sigNature = sigNature
        and  program = program
    end
end;

