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


functor PARSER (

structure Symbols : SYMBOLS
structure Lex : LEX
structure Parsetree : PARSETREE
sharing Symbols =  Lex.Sharing

) : PARSER =

struct

    open Symbols
    open Lex
    open Parsetree

    fun getsym (sym, lex) = 
        if (sy lex) = sym then insymbol lex else raise Fail ("Expected " ^ repr sym ^ " but found " ^ repr(sy lex))

    fun getList (separator, lex, each) =
    let
        fun forList list =
        let
            val item = each()
        in
            (* Add each item to the list.  The final span is from
               the start to the final location.  Make the list and then
               reverse it.  That's avoids quadratic performance on long lists. *)
            if sy lex = separator
            then (insymbol lex; forList(item :: list))
            else List.rev(item :: list)
        end
    in
        forList []
    end

    fun getid lex = 
    if sy lex = Ident orelse sy lex = Asterisk
    then id lex before insymbol lex
    else raise Fail "Expected identifier"

    fun getTypeVars lex =
    let
        (* Optional type identifier or sequence of type identifiers.  Used
           in type and datatype declarations and also in val and fun
           declarations. *)
        fun getTypeVar () =
        (* Read a type variable and return it. *)
        case sy lex of
            TypeIdent => id lex before insymbol lex
        |   _ => raise Fail "Expected a type var"
    in
        (* May be one type variable or a bracketed sequence. *)
        case sy lex of
            TypeIdent => [getTypeVar()] (* One type var. *)
        |   LeftParen => (* Sequence. *)
            (
                insymbol lex;
                case sy lex of
                    TypeIdent =>
                    let
                        val t = getList (Comma, lex, getTypeVar)
                    in
                        getsym (RightParen, lex);
                        t
                    end
                |   _ => (pushBackSymbol(lex, LeftParen); [] )
            )
        |   _ => [] (* None at all. *)
    end; (* getTypeVars *)

    fun parseType lex =
    let
        fun tupleType () =
        let
            fun basicType () =
            let (* First part may be a type sequence. *)
                val sym = sy lex 
                val (tySeq) =
                    case sym of
                        LeftParen => (* sequence of types *)
                        let
                            fun processList () =
                            let
                                val thisType = 
                                    if sy lex = TypeIdent orelse sy lex = Ident orelse sy lex = LeftParen orelse sy lex = LeftCurly
                                    then parseType lex
                                    else raise Fail "Expected type ident"

                            in (* Check for any more *)
                                if sy lex = Comma
                                then (insymbol lex; thisType :: processList()) (* get some more *)
                                else [thisType] (* that's it *)
                            end (* processList *);

                            val ()      = insymbol lex;  (* Remove opening bracket *)
                            val sequence = processList(); (* read list of items *)
                        in
                            getsym (RightParen, lex);
                            sequence
                        end

                    |   LeftCurly =>
                        let
                            val () = insymbol lex; (* Remove opening bracket *)
                        in
                            case sy lex of
                                RightCurly =>
                                let
                                    val () = insymbol lex
                                in
                                    [UnitTree]
                                end
        
                            |   _ =>
                                let
                                    fun getLabel lex = 
                                    if (sy lex) = IntegerConst
                                    then
                                    let
                                        val iden = id lex
                                    in
                                        insymbol lex;
                                        iden
                                    end
                                    else getid lex

                                    (* All the labels should be the same sort. *)
                                    val l = 
                                        getList (Comma, lex,
                                            fn () =>
                                            let
                                                val name = getLabel lex
                                                val () = getsym (Colon, lex)
                                                val types = parseType lex
                                            in
                                                (name, types)
                                            end)
                                in
                                    getsym (RightCurly, lex);
                                    [Labelled l]
                                end
                        end
                    
                    |   TypeIdent =>
                        let (* type variable *)
                            val tyVarName = id lex
                        in
                            getsym (TypeIdent, lex);
                            [TypeVariable tyVarName]
                        end
      
                    |   Ident => (* Constructor such as `int' *)
                            [TypeConstruction(getid lex, [])]

                    |   _ =>
                        (
                            raise Fail "Expected type";
                            []
                        )
            in
                (* Type sequence read. Can now have some type constructors. *)
                case (sy lex, tySeq) of
                    (Ident, _) =>
                    let (* Returns the type made from the constructors. *)
                        fun constructors(args) =
                        let
                            val id = id lex
                            val constructed = TypeConstruction(id, args);
                        in
                            insymbol lex;
                            if sy lex = Ident
                            then constructors([constructed])
                            else (constructed)
                        end;
                    in
                        constructors(tySeq)
                    end

                (* no constructor - get the first part of the sequence
                   and check that that's all. *)
                |   (_, [])   => raise Fail "Expected a type"
                |   (_, [t])  => (t)
                |   (_, _::_) => raise Fail "Expected single type"
            end (* basicType *);

            (* ty * .. * ty  *)
            fun getProduct () =
            let
                val firstPart = basicType ()
            in
                case sy lex of
                    Asterisk =>
                    let
                        val () = insymbol lex
                        val (rest) = getProduct ()
                    in
                        (firstPart :: rest)
                    end
                |   _ => [firstPart]
            end
        in
            case getProduct () of
                ([notProduct]) => (notProduct)
            |   (product) => TypeProduct(product)
        end  (* tupleType *)(* ty -> ty *)
   
        val (firstType) = tupleType ();
    in
        case sy lex of
            Arrow =>
            let
                val () = insymbol lex
                val resType = parseType lex
            in
                TypeFunction (firstType, resType)
            end
        |   _ => firstType
    end

    fun typeBinding lex =
    let
        val typeVars = getTypeVars lex
        (* The name of the type *)
        val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
        val typeName = id lex before insymbol lex
        val matchedType =
          (* If this is part of a signature we do not need to have an
           "= ty" after it. If it is an eqtype we must not have one. *)
            if (sy lex) <> EqualsSign
            then NONE
            else
            let
                val () = getsym (EqualsSign, lex);
                (* Followed by a type or a sequence of constructors *)
                val t = parseType lex
            in
                SOME t
            end
    in
        (typeName, typeVars, matchedType)
    end (* typeBinding *);
    
   fun datatypeDecOrRepl lex =
   (* "datatype" has been read.  This may be followed by tycon = datatype ...
      if it is a datatype replication or by tyvarseq tycon = vid ... if it
      is a datatype binding.  We can only distinguish the two when we reach
      either the second datatype or an identifier.
      This is used both for declarations and for specifications. *)
      let
            val () = insymbol lex;
            val text = getText lex
            (* The type variables will be empty if this is a replication. *)
            val typeVars = getTypeVars lex
            (* The name of the type *)
            val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
            val typeName = id lex before insymbol lex

            val () = getsym (EqualsSign, lex);
        in
        case sy lex of
            DatatypeSy => (* Replication *)
            let
                val () = insymbol lex;
                val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
                val originalTypeName = id lex before insymbol lex
            in
                [DatatypeReplication{newType=typeName, oldType=originalTypeName}]
            end
        |   _ => (* Binding *)
            let
                (* Process the rest of this binding. *)
                val db = datatypeBind (typeName, typeVars, lex)
                (* Process any others *)
                val dbs =
                    case sy lex of
                        AndSy => 
                            (
                                insymbol lex;
                                getList (AndSy, lex, fn () => datatypeBinding lex)
                            )
                    |   _ => []

                val dtSpec = [DatatypeSpec(db :: dbs, text)]
                (* Withtype is probably not allowed in a signature *)
                val withTypes =
                    if sy lex = WithtypeSy
                    then
                    (
                        insymbol lex;
                        [TypeSpec{items=getList (AndSy, lex, fn () => typeBinding lex), typeKind=TypeKindWithType, text=text}]
                    )
                    else []
            in
                dtSpec @ withTypes
            end
    end
    
    and datatypeBind (typeName, typeVars, lex): string * string list * (string * typeParseTree option) list =
    (* Process the rest of a binding. *)
    let
        fun constrs () =
        let
            (* Identifier - name of constructor *)
            val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
            val constrName = id lex before insymbol lex
              
            (* If there is an "of" after this then the constructor is
                a function from the type following the "of" to the type
                being declared. Otherwise it is a constant of the type 
                being declared. *)
            val component =
                case sy lex of
                    OfSy =>
                    let
                        val () = insymbol lex;(* Followed by a type. *)
                        val theType = parseType lex
                    in
                        (constrName, SOME theType)
                    end
                |   _ => (constrName, NONE)
        in
            case sy lex of
                VerticalBar =>
                let
                    val () = insymbol lex
                    val tail = constrs ()
                in
                    component :: tail
                end
            |   _ => [component]
         end
         
        val constrs = constrs ()
    in
        (typeName, typeVars, constrs)
    end

    and datatypeBinding lex =
    (* Datatype and abstype declarations and datatype specifications. *)
    let
        val typeVars = getTypeVars lex
        (* The name of the type *)
        val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
        val typeName = id lex before insymbol lex

        val () = getsym (EqualsSign, lex);
    in
        datatypeBind (typeName, typeVars, lex)
    end

    (* Parses a signature. *)
    fun parseSignature lex =
    let  (* May be either a signature name or a sig spec .. spec end seq
          followed by multiple  where type  expressions. *)

        val sigexp: sigNature =
            case sy lex of
                SigSy =>
                let (* sig *)
                    val () = insymbol lex
                    val sigs = signatureSpec lex
                in
                    getsym (EndSy, lex);
                    SigEnd sigs
                end

            |   Ident =>
                let
                    val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
                    val name = id lex before insymbol lex
                in
                    NamedSig name
                end

            |   _ => (* Only if parse error which will have been reported in skipon. *)
                raise Fail "not a signature"

        fun getWhereTypes sigexp =
        let
            (* This is similar to a type binding but with the possibility
               that the type is a longtycon. *)
            val () = getsym(TypeSy, lex);
            val typeVars = getTypeVars lex
            val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
            val name = id lex before insymbol lex
            val () = getsym (EqualsSign, lex)
            val srcType = TypeConstruction(name, map TypeVariable typeVars)
            (* Followed by a type or a sequence of constructors *)
            val theType = parseType lex
            val constrainedSig = SigWhere(sigexp, srcType, theType)
        in
            case sy lex of
                WhereSy => (* Recurse to handle any other wheres. *)
                    (insymbol lex; getWhereTypes constrainedSig)

            |   AndSy =>
                (
                    insymbol lex;
                    (* There are two possibilities here.  It may be the start of another
                       type abbreviation or it may be the start of another signature. *)
                    case sy lex of
                        TypeSy => constrainedSig
                    |   _ => (* Push the "and" back into the lexer so it can be picked out later. *)
                    (
                        pushBackSymbol(lex, AndSy);
                        constrainedSig
                    )
                )
            |   _ => constrainedSig
        end
    in
        case sy lex of
            WhereSy => (insymbol lex; getWhereTypes sigexp)
        |   _ => sigexp
    end (* parseSignature *)

    and signatureSpec lex =
    let
       val sym = sy lex
       val thisSig =
       case sym of
            DatatypeSy =>
            let
            in
                datatypeDecOrRepl lex
            end

         |  TypeSy =>
            let
                val () = insymbol lex
                val text = getText lex
            in
                [TypeSpec{items=getList (AndSy, lex, fn () => typeBinding lex), typeKind=TypeKindType, text=text}]
            end

         |  EqtypeSy =>
            let
                val () = insymbol lex
                val text = getText lex
            in
                [TypeSpec{items=getList (AndSy, lex, fn () => typeBinding lex), typeKind=TypeKindEqType, text=text}]
            end

         |  ValSy =>
            let
                val () = insymbol lex
                val text = getText lex
         
                fun doVal () =
                let
                    val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
                    val name = id lex before insymbol lex
                    val () = getsym (Colon, lex);
                    val ty =  parseType lex
               in
                    (name, ty)
               end
           in
                [ValSpec(getList (AndSy, lex, doVal), text)]
           end (* val *)

        |   ExceptionSy =>
            let(* exception id1 of ty1 and _ and idn of tyn *)
                val () = insymbol lex
                val text = getText lex
         
                fun doEx () =
                let
                    val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
                    val name = id lex before insymbol lex

                    val tyOption =
                        case sy lex of
                            OfSy =>
                            let
                                val () = insymbol lex
                                val types = parseType lex
                            in
                                (SOME types)
                            end
                        |   _ => (* Nullary *) (NONE);
               in
                     (name, tyOption)
               end
            in
                [ExSpec(getList (AndSy, lex, doEx), text)]
            end (* exception *)
       
         |  StructureSy =>
            let
                val () = insymbol lex
                val text = getText lex

                fun doStructure () =
                let
                    val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
                    val name = id lex before insymbol lex
                    val () = getsym (Colon, lex)
                    val (sgn) = parseSignature lex
                in
                    (name, sgn)
                end
            in
                [StructSpec(getList(AndSy, lex, doStructure), text)]
            end

         |  IncludeSy =>
            let
                val () = insymbol lex

                val firstSig = parseSignature lex

                fun sigids () =
                    case sy lex of
                        Ident =>
                        let
                            val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
                            val name = id lex before insymbol lex
                            val rest = sigids ()
                        in
                            NamedSig name :: rest
                        end
                    |   _ => []

                val (otherSigs) = sigids ()
            in
                [IncludeSig (firstSig :: otherSigs)]
            end

         |  SharingSy =>
            let (* sharing *)
                val () = insymbol lex
                val isType = case sy lex of TypeSy => (insymbol lex; true) | _ => false
                fun getShare () =
                let
                    val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
                in
                    id lex before insymbol lex
                end
                val shares = getShare ()
                val () = getsym (EqualsSign, lex)
                val shareRest = getList (EqualsSign, lex, getShare)
            in
                [Sharing{isType=isType, shares=shares :: shareRest}]
            end

        |   _ => [] (* Empty. *)
           (* end of parse of thisSig *)
         
        (* continue until the `end' *)
        val () = case sy lex of Semicolon => insymbol lex | _ => ()
    in 
        if sy lex = Semicolon orelse sy lex = DatatypeSy orelse sy lex = TypeSy orelse sy lex = EqtypeSy orelse
            sy lex = StructureSy orelse sy lex = SharingSy orelse sy lex = IncludeSy orelse sy lex = ValSy orelse
            sy lex = ExceptionSy
        then thisSig @ signatureSpec lex
        else thisSig
    end 

    fun signatureDec lex =
    let
        val () = insymbol lex
        fun doSigDec () =
        let
            val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
            val sigName = id lex before insymbol lex
            val () = getsym (EqualsSign, lex)
            val sgn = parseSignature lex
        in
            (sigName, sgn)
        end
    in
        Signature(getList (AndSy, lex, doSigDec))
    end

    and skipToEnd 0 _ = ()
    |   skipToEnd n lex =
    case sy lex of
        EndSy => (insymbol lex; skipToEnd (n-1) lex)
    |   AbortParse => raise Fail "Looking for end but found end-of-file"
    |   StructSy => (insymbol lex; skipToEnd (n+1) lex)
    |   LetSy => (insymbol lex; skipToEnd (n+1) lex)
    |   LocalSy => (insymbol lex; skipToEnd (n+1) lex)
    |   AbstypeSy => (insymbol lex; skipToEnd (n+1) lex)
    |   _ => (insymbol lex; skipToEnd n lex)

    and structureDec lex =
    let
        val () = insymbol lex
        fun doStructDec () =
        let
            val () = if sy lex = Ident orelse sy lex = Asterisk then () else raise Fail "Expected id"
            val strName = id lex before insymbol lex
            val sgn =
                if sy lex = Colon orelse sy lex = ColonGt
                then (insymbol lex; SOME(parseSignature lex))
                else NONE
                
            val () = getsym (EqualsSign, lex)
        in
            (* Skip the structure body *)
            if sy lex = StructSy
            then (insymbol lex; skipToEnd 1 lex)
        
            else if sy lex = Ident orelse sy lex = Asterisk
            then insymbol lex
            else raise Fail "Expected struct or structure name";

            (strName, sgn)
        end
    in
        Structure(getList (AndSy, lex, doStructDec))
    end

    fun parseTopDecs lex =
    let
        val preText = getText lex
        
        fun parseTop lex =
            if sy lex = AbortParse (* Until end-of-file *)
            then []

            else if sy lex = SignatureSy
            then signatureDec lex :: parseTop lex
    
            else if sy lex = StructureSy
            then structureDec lex :: parseTop lex
    
            else if sy lex = Semicolon
            then (insymbol lex; parseTop lex)
            
            else if sy lex = LocalSy
            then
            let
                val _ = insymbol lex
                val _ = parseTop lex
                val _ = getsym(InSy, lex)
                val lDecs = parseTop lex
                val _ = getsym(EndSy, lex)
            in
                lDecs @ parseTop lex
            end
            
            else if sy lex = EndSy orelse sy lex = InSy
            (* If we find these inside a local we're done. *)
            then []

            else
            let
                (* Skip other top-decs e.g val *)
                fun skipTop lex =
                (
                    case sy lex of
                        Semicolon => ()
                    |   AbortParse => ()
                    |   EndSy => () (* Inside Local *)
                    |   InSy => () (* Inside Local *)
                    |   StructureSy => () (* Start of new structure inside Local *)
                    |   StructSy => (insymbol lex; skipToEnd 1 lex; skipTop lex)
                    |   LetSy => (insymbol lex; skipToEnd 1 lex; skipTop lex)
                    |   LocalSy => (insymbol lex; skipToEnd 1 lex; skipTop lex)
                    |   AbstypeSy => (insymbol lex; skipToEnd 1 lex; skipTop lex)
                    |   _ => (insymbol lex; skipTop lex)

                )
            in
                skipTop lex;
                parseTop lex
            end

        val decs = parseTop lex
        val postText = getText lex
    in
        { preText = preText, decs = decs, postText = postText }
    end

    val parseDec = parseTopDecs

    structure Sharing =
    struct
        type lexan = lexan
        and program = program
    end
end;

