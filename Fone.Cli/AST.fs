module Fable.C.AST

[<RequireQualifiedAccess>]
module rec C =
    // todo: Storage classes:  https://www.tutorialspoint.com/cprogramming/c_storage_classes.htm

    // https://www.tutorialspoint.com/cprogramming/c_operators.htm
    type DeclarationInfo = { _type: Type; name: string; value: DeclarationAssignment; requiresTracking: bool }

    type DeclarationAssignment =
        | ExprAssignment of Expr
        | StatementAssignment of Statement
        | ComplexAssignment
        | Default

    type Type =
        | Void
        | Byte
        | Int
        | UInt32
        | Int16
        | UInt16
        | Int64
        | UInt64
        | UIntptr_t
        | Float
        | Double
        | Bool
        | Char
        | UnsignedChar
        | UserDefined of fullName: string * isValueType: bool * entityRef: Fable.AST.Fable.Entity option
        // todo: user defined custom types
        | Ptr of ref: Type
        | Generic of atom: string
        | EmitType of string
        | FunctionPtr of args: Type list * returnType: Type
        with
        member this.ToTypeString() =
            match this with
            | Void -> "void"
            | UIntptr_t -> "uintptr_t"
            | Byte -> "unsigned char"
            | Int -> "int"
            | Int64 -> "long"
            | UInt64 -> "unsigned long"
            | UInt32 -> "unsigned int"
            | Int16 -> "short"
            | UInt16 -> "unsigned short"
            | Float -> "float"
            | Double -> "double"
            | Bool -> "bool"
            | Char -> "char"
            | UnsignedChar -> "unsigned char"
            | UserDefined (fullName, _, _) ->
                if fullName.StartsWith "struct " then fullName else "struct " + fullName
            | Ptr t -> $"{t.ToTypeString()}*"
            | EmitType string -> string
            | FunctionPtr(types, returnType) ->
                let s = System.String.Join(", ", types |> List.map (fun t -> t.ToTypeString()))
                $"{returnType.ToTypeString()} (*fn)({s})"
            | Generic name -> name
        member this.ToNameString() =
            match this with
            | Void -> "void"
            | UIntptr_t -> "uintptr_t"
            | Byte -> "byte"
            | Int -> "int"
            | Int16 -> "short"
            | UnsignedChar -> "unsigned_char"
            | UInt32 -> "unsigned_int"
            | UInt16 -> "unsigned_short"
            | Int64 -> "long"
            | UInt64 -> "unsigned_long"
            | Float -> "float"
            | Double -> "double"
            | Bool -> "bool"
            | Char -> "char"
            | FunctionPtr (args, return_type) -> "function_ptr" // todo
            | Generic name -> "generic_arg_" + name
            | UserDefined (fullName, _, _) -> fullName
            | Ptr t -> $"{t.ToNameString()}ptr"
            | EmitType string ->
                // let name = string.Replace("struct ", "")
                // name
                string.Replace(" ", "_")
        member this.PrintfType =
            match this with
            | Void -> "%d"
            | Int64 | UInt64  -> "%ld"
            | UInt32 -> "%u"
            | Byte | Int -> "%d"
            | Float | Double -> "%f"
            | Bool -> "%b"
            | Char -> "%c"
            | UserDefined (fullName, _, _) -> "%d"
            | Ptr t ->
                match t with
                | Char ->
                    "%s"
                | _ ->
                    "%p"
            | _ -> "%p"

    // type Statement = { Expr: Fable.AST.Fable.Expr; Statement: StatementKind }
    // and StatementKind =
    // [<RequireQualifiedAccess>]
    // type E<'t> =
    //     | Emit of string
    //     | Assignment of 't * 't
    //     | WhileLoop of 't * E<'t> list
    //     | ForLoop of DeclarationInfo * 't * E<'t> * E<'t> list
    //     | DoWhileLoop of 't * E<'t> list
    //     | Conditional of guard: 't * ifTrue: E<'t> list * ifFalse: E<'t> list
    //     | Declaration of decl: DeclarationInfo
    //     | Expression of 't
    //     | Block of E<'t> list
    //     | Return of 't
    // type CompiledStatement = Fable.AST.Fable.Expr * C.Expr
    // type T<'t> =
    //     | Emit of string
    //     | Assignment of Expr * Expr
    //     | WhileLoop of Expr * T<'t> list
    //     | ForLoop of DeclarationInfo * Expr * 't * T<'t> * 't * T<'t> list
    //     | DoWhileLoop of Expr * T<'t> list
    //     | Conditional of guard: Expr * 't * ifTrue: T<'t> list * 't * ifFalse: T<'t> list
    //     | Declaration of decl: DeclarationInfo
    //     | Expression of Expr
    //     | Block of T<'t> list
    //     | Return of Expr
    // type C = string * T<string>
    and Statement =
        | Emit of string
        | Assignment of Expr * Expr
        | WhileLoop of Expr * Statement list
        | ForLoop of DeclarationInfo * Expr * Statement * Statement list
        | DoWhileLoop of Expr * Statement list
        | Conditional of guard: Expr * ifTrue: Statement list * ifFalse: Statement list
        | Declaration of decl: DeclarationInfo
        | Expression of Expr
        | Block of Statement list
        | Return of Expr
        with
        // static member FromE(e: E<Fable.AST.Fable.Expr * Expr>) : Statement =
        //     match e with
        //     | E.Emit s -> Emit s
        //     | E.Assignment (a, b) -> Assignment (snd a, snd b)
        //     | E.WhileLoop (a, b) -> WhileLoop(snd a, List.map Statement.FromE b)
        //     | E.ForLoop (info, a, b, c) -> ForLoop(info, snd a, Statement.FromE b, List.map Statement.FromE c)
        //     | E.DoWhileLoop (a, b) -> WhileLoop(snd a, List.map Statement.FromE b)
        //     | E.Conditional (guard, ifTrue, ifFalse) -> Conditional(snd guard, List.map Statement.FromE ifTrue, List.map Statement.FromE ifFalse)
        //     | E.Declaration decl -> Declaration decl
        //     | E.Expression e -> Expression (snd e)
        //     | E.Block block -> Block <| List.map Statement.FromE block
        //     | E.Return e -> Return <| snd e
        member this.RequiresDestructor =
            match this with
            // todo: the right way to do this is to look at the call to make the assignment. if it's a function that returns
            // an obj that requires GC, then yes
            | Declaration decl when decl.name <> "this$" ->
                match decl._type with
                | Type.Ptr (UserDefined (fullName, isValueType, e))
                | UserDefined (fullName, isValueType, e) when not isValueType || (e.IsSome && not e.Value.IsValueType) ->
                    let index = 0 //fullName.IndexOf("_")
                    let name = fullName.Substring(index + 1, fullName.Length - index - 2)
                    Some ($"{fullName}", decl.name) // todo: lookup if this is actually a destructor type
//                | Type.Ptr p ->
//                    match p with
//                    | UserDefined (fullName, _, entityRef) when not entityRef.IsValueType ->
//                        let index = 0 //fullName.IndexOf("_")
//                        let name = fullName.Substring(index + 1, fullName.Length - index - 1)
//                        Some ($"{fullName}", decl.name) // todo: lookup if this is actually a destructor type
//                    | _ ->
//                        None
                | _ ->
                    None
            | _ ->
                None


    type UnaryOp =
        | Deref
        | Ref
        | Not
        | BinaryOneCompliement

    type UnaryExpr = UnaryOp * Expr

    type BinaryOp =
        // Math
        | Add
        | Minus
        | Mult
        | Div
        | Modulo
        | Power
        // Logic
        | And
        | Or
        // Relational
        | Eq
        | NotEq
        | Greater
        | Less
        | GreaterOrEqual
        | LessOrEqual

    type BinaryExpr = BinaryOp * Expr * Expr

    type ValueKind =
        | Void
        | Byte of byte
        | Int of int
        | Int16 of int16
        | UInt16 of uint16
        | UInt32 of uint
        | UInt64 of uint64
        | Float of float32
        | Double of double
        | Bool of bool
        | Char of char
        | CStr of string
        | Ptr of uint64
        | Emit of string
        | UnionConstructorCall of Expr
        | Compound of Expr list * Fable.AST.Fable.Entity * (string * Type) list * genericArgs: (Type list)
        | AnonymousCompound of values: Expr list * ent: Fable.AST.Fable.Entity * genericArgs: Type list
        | ObjectCompound of structName: string * values: Expr list
        | Array of Expr list

    type Ident = Fable.AST.Fable.Ident
    type Expr =
        | SizeOf of Type
        | Ref of Expr
        | ExprAssignment of destination: Expr * Expr
        | Binary of BinaryExpr
        | Call of name: string * args: Expr list
        | Unary of UnaryExpr
        | Ident of Ident * isValueType: bool
        | Ternary of cond: Expr * onTrue: Expr * onFalse: Expr
        | MemberAccess of ident: Expr * field: string
        | DerefMemberAccess of ident: Expr * field: string
        | IndexedAccess of source: Expr * index: Expr
        | Value of ValueKind
        | TypeCast of Type * Expr
        | BlockExpr of Statement list
        | Emit of string
        | Unknown // todo: of Fable.AST.Fable.Expr

//    type TypeDecl =
//        | Struct of fields: (string * Type) list

    type ModuleDeclaration =
        | Extern of name: string * args: (string * Type) list * return_type: Type
        | Function of FunctionInfo
        | Struct of Struct
        | StaticVar of DeclarationInfo
        | Union of cases: Struct list

    type ModuleInfo =
        { structs: Map<string, Struct>
          unions: Map<string, Ident list>
          variables: Map<string, Fable.AST.Fable.Declaration>
          functions: Map<string, FunctionInfo> }

    type FunctionInfo =
        { id: string; return_type: Type; args: (string * Type) list; body: Statement list }

    type Module =
        { imports: string list; statics: string list; functions: string list }

    type Struct =
        { tag: string; members: (Type * string) list } // todo: array literals like char title[50]; see: https://www.tutorialspoint.com/cprogramming/c_structures.htm

type Web =
    | Foo
