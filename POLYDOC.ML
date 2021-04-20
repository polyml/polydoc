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

functor POLYDOC
(
structure Lex : LEX
and       Parsetree : PARSETREE
and       Parser : PARSER
and       Markdown: MARKDOWN

sharing Lex.Sharing = Parsetree.Sharing = Parser.Sharing
) =
struct

    exception Error

    fun buildPage{source, styleSheet, outputFile, previous, next, parent} =
    let
        open TextIO Parsetree
        val streamIn =
            openIn source
                handle IO.Io _ =>
                (
                    output(stdErr, "Unable to open source file: " ^ source ^ "\n");
                    raise Error
                )
        val line = ref 1

        fun getCh() =
        let
            val ch = TextIO.input1 streamIn
            val () = case ch of SOME #"\n" => line := !line + 1 | _ => ()
        in
            ch
        end

        val lexer = Lex.initial getCh
        val () = Lex.insymbol lexer
        
        val {decs, preText, postText} =
            Parser.parseDec lexer before TextIO.closeIn streamIn
                handle exn =>
                (
                    output(stdErr, concat[source, ", ", Int.toString(!line), ": Exception during parsing ", exnMessage exn, "\n"]);
                    TextIO.closeIn streamIn;
                    raise Error
                )

        val outstream =
            TextIO.openOut outputFile
                handle IO.Io _ =>
                (
                    output(stdErr, "Unable to open output file: " ^ outputFile ^ "\n");
                    raise Error
                )

        fun out s = output(outstream, s)

        fun makeNav(kind, SOME(link, name)) =
                out(concat["<", kind, " href=\"", link, "\">", name, "</", kind, ">\n"])
        |   makeNav(_, NONE) = ()

        local
            fun createName str (s, _) = str ^ " " ^ s
            fun createFnName str (s, _, _) = str ^ " " ^ s
            val sigNames =
                List.foldl(fn (Signature s, l) =>
                    map (createName "Signature") s @ l | (_, l) => l) [] decs
            val structNames =
                List.foldl(fn (Structure s, l) =>
                    map (createName "Structure") s @ l | (_, l) => l) [] decs
            val functorNames =
                List.foldl(fn (Functor s, l) =>
                    map (createFnName "Functor") s @ l | (_, l) => l) [] decs
        in
            val title = String.concatWith " " (sigNames @ structNames @ functorNames)
        end
 
    in
        out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
        
        case styleSheet of
            SOME style =>
                out(concat["<?xml-stylesheet type=\"text/xsl\" href=\"", style, "\"?>\n"])
        |   NONE => ();
        out "<polydoc>\n";
        out "<title>"; out title; out "</title>\n";
        
        out "<navigation>\n";
        makeNav("parent", parent); 
        makeNav("previous", previous); 
        makeNav("next", next); 
        out "</navigation>\n";

        out "<headtext>\n";
        Markdown.outputMarkdown(outstream, preText);
        out "</headtext>\n";
        
        out "<topdecs>\n";
            outputProgram(decs, outstream);
        out "</topdecs>\n";
        
        out "<tailtext>\n";
        Markdown.outputMarkdown(outstream, postText);
        out "</tailtext>\n";

        out "</polydoc>\n";
        closeOut outstream
    end
        handle Error => () (* Error reported elsewhere. *)
            |   exn =>
                    TextIO.output(TextIO.stdErr, "Unexpected exception " ^ exnMessage exn ^ "\n")
    local
        open OS.Path OS.FileSys
        fun addXml s = joinBaseExt{base=s, ext=SOME "xml"}
        
        (* Convert the source name into an output file name.
           We keep the original extension but add .xml *)
        fun sourceNameToHTML(outputDir, sourceFile) =
            joinDirFile{dir=outputDir, file=addXml(file sourceFile)}
    in
        fun processFiles{sourceFiles=[], ...} =
            raise Fail "No source files provided"
    
        |   processFiles{sourceFiles=[single], styleSheet, outputFileOrDir, parent} =
            if isDir single
            then processDirectory{directory=single, styleSheet=styleSheet,
                                  outputFileOrDir=outputFileOrDir, parent=parent}
            else
            let
                (* The output may name a file or a directory *)
                val outputFile =
                    if isDir outputFileOrDir
                    then sourceNameToHTML(outputFileOrDir, single)
                    else outputFileOrDir
            in
                buildPage{source=single, styleSheet=styleSheet, outputFile=outputFile,
                          next=NONE, previous=NONE, parent=parent}
            end

        |   processFiles{sourceFiles, styleSheet, outputFileOrDir, parent} =
            let
                val listWithBaseNames =
                    map (fn path => (path, base(#file(splitDirFile path)))) sourceFiles
                (* Sort the files by file name, ignoring case *)
                fun quickSort [] = []
                |   quickSort [h] = [h]
                |   quickSort ((h as (_, name)) ::t) =
                    let
                        fun leq (_, s) =
                            case String.collate(
                                fn (c1, c2) => Char.compare(Char.toUpper c1, Char.toUpper c2)) (name, s) of
                                GREATER => false
                            |   _ => true
                        val (after, befor) = List.partition leq t
                    in
                        quickSort befor @ (h :: quickSort after)
                    end
                val sorted = quickSort listWithBaseNames
            
                fun processSources(_, []) = ()
            
                |   processSources(previous, [(path, _)]) =
                        buildPage{source=path, styleSheet=styleSheet,
                            outputFile=sourceNameToHTML(outputFileOrDir, path),
                            previous=previous, next=NONE, parent=parent}
                
                |   processSources(previous, (path, name) :: (rest as (nextPath, next) :: _)) =
                    (
                        buildPage{source=path, styleSheet=styleSheet,
                            outputFile=sourceNameToHTML(outputFileOrDir, path),
                            previous=previous, next=SOME(addXml(file nextPath), next), parent=parent};
                        processSources(SOME(addXml(file path), name), rest)
                    )
            in
                processSources(NONE, sorted)
            end
    
        (* Process all the ML files in a directory *)
        and processDirectory{directory, styleSheet, outputFileOrDir, parent} =
        let
            open OS.FileSys OS.Path
            val dir = openDir directory
                handle OS.SysErr _ => raise Fail("Could not open directory: " ^ directory)
            fun readAll l =
                case readDir dir of
                    SOME f =>
                    (
                        case #ext(splitBaseExt f) of
                            NONE => readAll l
                        |   SOME extension =>
                                if List.exists(fn s => s = extension) ["ML", "sml", "sig"]
                            then readAll(joinDirFile{dir=directory, file=f} :: l)
                            else readAll l
                    )
                |   NONE => l

            val allFiles = readAll []
            val () = closeDir dir
        in
            processFiles{sourceFiles=allFiles, styleSheet=styleSheet,
                 outputFileOrDir=outputFileOrDir, parent=parent}
        end
    end

end;
