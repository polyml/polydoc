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

signature PARSETREE =
sig

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

    val outputProgram: program list * TextIO.outstream -> unit
    
    structure Sharing:
    sig
        type typeParseTree = typeParseTree
        and  specParseTree = specParseTree
        and  sigNature = sigNature
        and  program = program
    end
end;
