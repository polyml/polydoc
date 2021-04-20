(*
    Copyright (c) 2021 David C.J. Matthews

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

fun main() =
let
    fun error message = (TextIO.output(TextIO.stdErr, message); OS.Process.exit OS.Process.failure)
    
    fun help() =
    (TextIO.output(TextIO.stdErr,
        "Usage: polydoc [-o output] [-s stylesheet] infile..."); OS.Process.exit OS.Process.success)

    fun processArgs(result, []) = result
    
    |   processArgs(_, "-h" :: _) = help()
    |   processArgs(_, "--help" :: _) = help()

    |   processArgs({sourceFiles, styleSheet, outputFileOrDir, parent},
            "-o" :: outFile :: args) =
            if outputFileOrDir = ""
            then processArgs({sourceFiles=sourceFiles, styleSheet=styleSheet, outputFileOrDir=outFile, parent=parent}, args)
            else error "-o argument may only appear once\n"

    |   processArgs({sourceFiles, styleSheet, outputFileOrDir, parent, ...},
            "-f" :: style :: args) =
        (
            case styleSheet of
                NONE =>
                    processArgs({sourceFiles=sourceFiles, styleSheet=SOME style, outputFileOrDir=outputFileOrDir, parent=parent}, args)
            |   SOME _ => error "-f argument may only appear once\n"
        )

    |   processArgs({sourceFiles, styleSheet, outputFileOrDir, parent}, inFile :: args) =
            if String.isPrefix "-" inFile
            then error ("Unknown option " ^ inFile)
            else processArgs({sourceFiles=sourceFiles @ [inFile], styleSheet=styleSheet, outputFileOrDir=outputFileOrDir, parent=parent}, args)
   
    val arguments =
        processArgs({sourceFiles=[], styleSheet=NONE, outputFileOrDir="", parent=NONE},
            CommandLine.arguments())
in
    PolyDoc.processFiles arguments;
    OS.Process.exit OS.Process.success
end;
