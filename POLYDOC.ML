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

    fun buildPage{source, template, outputFile, previous, next, parent} =
    let
        open TextIO Parsetree
        val streamIn =
            openIn source
                handle IO.Io _ =>
                (
                    output(stdErr, "Unable to open source file: " ^ source ^ "\n");
                    raise Error
                )
        fun getCh() = TextIO.input1 streamIn
        val lexer = Lex.initial getCh
        val () = Lex.insymbol lexer
        
        val {decs, preText, postText} =
            Parser.parseDec lexer before TextIO.closeIn streamIn
                handle exn =>
                (
                    output(stdErr, "Exception during parsing " ^ exnMessage exn ^ "\n");
                    TextIO.closeIn streamIn;
                    raise Error
                )

        fun makeNav(SOME(link, name)) =
                concat["<li><a href=\"", link, "\">", name, "</a></li>\n"]
        |   makeNav(NONE) = "<li></li>\n"

        val navigation =
            concat["<ul class=\"nav\">\n", makeNav previous,
                   makeNav parent, makeNav next, "</ul>"]

        (* Copy the input template to the output, replacing <!--INSERTION-->
           by the supplied information. *)
        val defaultTemplate =
            concat[
                "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">\n",
                "<HTML>\n<HEAD>\n<TITLE>\n<!--TITLE-->\n</TITLE>\n",
                "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n",
                "<link href=\"http://www.polyml.org/documentation/Reference/docstyle.css\" rel=\"stylesheet\" type=\"text/css\">\n",
                "</HEAD>\n<BODY BGCOLOR=\"#ffffff\">\n<!--NAVIGATION-->\n",
                "<H2><STRONG>\n<!--TITLE-->\n</STRONG></H2>\n<!--BODY-->\n",
                "<!--NAVIGATION-->\n</BODY>\n</HTML>"]

        val instream =
            case template of NONE => TextIO.openString defaultTemplate
            |   SOME template => TextIO.openIn template
                handle IO.Io _ =>
                (
                    output(stdErr, "Unable to open template file: " ^ template ^ "\n");
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
        
        local
            fun createName str (s, _) = str ^ " " ^ s
            val sigNames =
                List.foldl(fn (Signature s, l) =>
                    map (createName "Signature") s @ l | (_, l) => l) [] decs
            val structNames =
                List.foldl(fn (Structure s, l) =>
                    map (createName "Structure") s @ l | (_, l) => l) [] decs
        in
            val title = String.concatWith " " (sigNames @ structNames)
        end
 
        fun copyToOutput() =
            case inputLine instream of
                NONE => () (* Finished *)
            |   SOME s =>
                (
                    if s = "<!--BODY-->\n"
                    then
                    (
                        Markdown.outputMarkdown(outstream, preText);
                        outputProgram(decs, outstream);
                        Markdown.outputMarkdown(outstream, postText)
                    )
                    else if s = "<!--NAVIGATION-->\n"
                    then out navigation
                    else if s = "<!--TITLE-->\n"
                    then out title
                    else out s;
                    copyToOutput()
                )
    in
        copyToOutput();
        closeOut outstream;
        closeIn instream
    end
        handle Error => () (* Error reported elsewhere. *)
            |   exn =>
                    TextIO.output(TextIO.stdErr, "Unexpected exception " ^ exnMessage exn ^ "\n")
    local
        open OS.Path OS.FileSys
        fun addHtml s = joinBaseExt{base=s, ext=SOME "html"}
        
        (* Convert the source name into an output file name.
           We keep the original extension but add .html *)
        fun sourceNameToHTML(outputDir, sourceFile) =
            joinDirFile{dir=outputDir, file=addHtml(file sourceFile)}
    in
        fun processFiles{sourceFiles=[], ...} =
            raise Fail "No source files provided"
    
        |   processFiles{sourceFiles=[single], template, outputFileOrDir, parent} =
            if isDir single
            then processDirectory{directory=single, template=template,
                                  outputFileOrDir=outputFileOrDir, parent=parent}
            else
            let
                (* The output may name a file or a directory *)
                val outputFile =
                    if isDir outputFileOrDir
                    then sourceNameToHTML(outputFileOrDir, single)
                    else outputFileOrDir
            in
                buildPage{source=single, template=template, outputFile=outputFile,
                          next=NONE, previous=NONE, parent=parent}
            end

        |   processFiles{sourceFiles, template, outputFileOrDir, parent} =
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
                        buildPage{source=path, template=template,
                            outputFile=sourceNameToHTML(outputFileOrDir, path),
                            previous=previous, next=NONE, parent=parent}
                
                |   processSources(previous, (path, name) :: (rest as (nextPath, next) :: _)) =
                    (
                        buildPage{source=path, template=template,
                            outputFile=sourceNameToHTML(outputFileOrDir, path),
                            previous=previous, next=SOME(addHtml(file nextPath), next), parent=parent};
                        processSources(SOME(addHtml(file path), name), rest)
                    )
            in
                processSources(NONE, sorted)
            end
    
        (* Process all the ML files in a directory *)
        and processDirectory{directory, template, outputFileOrDir, parent} =
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
            processFiles{sourceFiles=allFiles, template=template,
                 outputFileOrDir=outputFileOrDir, parent=parent}
        end
    end

end;
