module rec Fable.C.Helpers

open System
open System.Text
open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.C.AST
open Fable.Transforms.FSharp2Fable
open Microsoft.FSharp.Reflection
open Fable.C.C99Compiler

type Compiler = Fable.Compiler

let inline (:=) a b =
    (^a : (member set_Value : ^b -> unit) (a, b))

let inline (+=)< ^ref, ^key, ^value
    when ^ref : (member Value : Map< ^key, ^value >)
//    and ^map : (member Add : ^tpl -> ^map)
    and ^ref : (member set_Value : Map< ^key, ^value > -> unit) >
        (hasValueAndAdd: ^ref) (tpl: ^key * ^value) =
    let map = (^ref : (member Value : Map< ^key, ^value>) hasValueAndAdd)
    let result = map.Add tpl // (^map : (member Add : ^tpl -> ^map) (map, tpl))
    hasValueAndAdd := result

//let inline (+=) a b =
//    let value = (^a : (member Value: ^b) a)
//    a := value + b
    
//let inline (@+) map tpl =
    
//    (^map : (member Add : ^tpl -> ^map) (map, tpl))
//    (^ref : (member set_Value : ^map -> unit) (hasValueAndAdd, result))

//Map.empty.Add
//(Map.empty) @+ ("", "")
//(ref 10) += 10
    

//let inline (+=) hasValueAndAdd argsForAddMethod =
//    let value = (^a : (member Value : ^b) hasValueAndAdd)
//    let result = (^b : (member Add : ^c -> ^b) (value, argsForAddMethod))
//    (^a : (member set_Value : ^b -> unit) (hasValueAndAdd, result))
let rec walkExprInPlace (f: Expr -> Expr) (expr: Expr) : Expr =
    let expr = f expr
    match expr with
    | Let(ident, value, body) ->
        Let(ident, walkExprInPlace f (f value), walkExprInPlace f (f body))
    | Sequential exprs ->
        Sequential (exprs |> List.map (walkExprInPlace f))
    | WhileLoop(guard, body, sourceLocationOption) ->
        WhileLoop(walkExprInPlace f (f guard), walkExprInPlace f (f body), sourceLocationOption)
    | IfThenElse(guardExpr, thenExpr, elseExpr, sourceLocationOption) ->
        IfThenElse(walkExprInPlace f (f guardExpr), walkExprInPlace f (f thenExpr), walkExprInPlace f (f elseExpr), sourceLocationOption)
    | Value(valueKind, sourceLocationOption) ->
        match valueKind with
        | NewRecord(values, entityRef, genArgs) ->
            Value(NewRecord(values |> List.map (walkExprInPlace f), entityRef, genArgs), sourceLocationOption)
        | NewAnonymousRecord(values, fieldNames, genArgs, isStruct) ->
            Value(NewAnonymousRecord(values |> List.map (walkExprInPlace f), fieldNames, genArgs, isStruct), sourceLocationOption)
        | StringTemplate(exprOption, parts, values) ->
            Value(StringTemplate(exprOption, parts, values |> List.map (walkExprInPlace f)), sourceLocationOption)
        | _ -> f expr
    | Set(expr, kind, ``type``, value, sourceLocationOption) ->
        Set(walkExprInPlace f (f expr), kind, ``type``, walkExprInPlace f (f value), sourceLocationOption)
    | Get(expr, kind, ``type``, sourceLocationOption) ->
        Get(walkExprInPlace f (f expr), kind, ``type``, sourceLocationOption)
    | Operation(operationKind, tags, ``type``, sourceLocationOption) ->
        match operationKind with
        | Binary(binaryOperator, left, right) ->
            Operation(Binary(binaryOperator, walkExprInPlace f left, walkExprInPlace f right), [], ``type``, sourceLocationOption)
        | Unary(unaryOperator, operand) ->
            Operation(Unary(unaryOperator, walkExprInPlace f operand), [], ``type``, sourceLocationOption)
        | Logical(logicalOperator, left, right) ->
            Operation(Logical(logicalOperator, walkExprInPlace f left, walkExprInPlace f right), [], ``type``, sourceLocationOption)
    | Call(callee, callInfo, ``type``, sourceLocationOption) ->
        let args = callInfo.Args |> List.map (walkExprInPlace f)
        Call(walkExprInPlace f callee, { callInfo with Args = args }, ``type``, sourceLocationOption)
    | Emit(emitInfo, ``type``, sourceLocationOption) ->
        expr
    | TypeCast(expr, ``type``) ->
        TypeCast (walkExprInPlace f expr, ``type``)
    | _ ->
        f expr
let rec walkExpr (f: Expr -> 'a) (expr: Expr) : 'a list =
    let mutable collector: 'a list = []
    let callback (e: Expr) =
//        log <| ($"%A{e}").Replace("\n", "; ")
        collector <- (f e) :: collector
        None
//    let visitorResult = Fable.Transforms.AST.visitFromInsideOut callback expr
    let outsideResult = Fable.Transforms.AST.visitFromOutsideIn callback expr
    collector |> List.rev
//    let result = f expr
//    let rest =
//        match expr with
//        | Let(ident, value, body) ->
//            f value :: (walkExpr f body)
//        | Sequential exprs ->
//            exprs |> List.collect (walkExpr f)
//        | WhileLoop(guard, body, sourceLocationOption) ->
//            f guard :: (walkExpr f body)
//        | IfThenElse(guardExpr, thenExpr, elseExpr, sourceLocationOption) ->
//            [ f guardExpr; f thenExpr; f elseExpr ]
//        | Value(valueKind, sourceLocationOption) ->
//            match valueKind with
//            | NewRecord(values, entityRef, genArgs) -> f expr :: (List.collect (walkExpr f) values)
//            | NewAnonymousRecord(values, fieldNames, genArgs, isStruct) -> f expr :: (List.collect (walkExpr f) values)
//            | _ -> []
//        | Set(expr, kind, ``type``, value, sourceLocationOption) ->
//            [ f expr; f value ]
//        | Call(callee, callInfo, ``type``, sourceLocationOption) ->
//            (walkExpr f callee) @ (List.collect (walkExpr f) callInfo.Args)
//        | _ ->
//            []
//    result :: rest

let rec gatherIdentsBefore (before: Expr) (expr: Expr) : Ident list =
    let deepExists = Fable.Transforms.AST.deepExists
    let rec loop acc expr =
        if expr = before then
            true, acc
        else
            match expr with
            | Let(ident, value, body) ->
                loop (acc @ [ident]) body
            | Sequential exprs ->
                let mutable acc = acc
                let mutable _done = false
                for e in exprs do
                    if e = before then _done <- true
                    elif not _done then
                        let (isDone, newlyAcc) = loop [] e
                        _done <- isDone
                        acc <- acc @ newlyAcc
                    else ()
                _done, acc
            | WhileLoop(guard, body, sourceLocationOption) ->
                if deepExists (fun _e -> _e = before) body then
                    loop acc body
                else
                    false, acc
            | ForLoop(ident, start, limit, body, isUp, sourceLocationOption) ->
                if deepExists (fun _e -> _e = before) body then
                    loop (acc @ [ident]) body
                else
                    false, acc
            | TryCatch (body, catch, finalizer, _) ->
                false, acc // todo
                
            // todo
            | LetRec _
            | DecisionTree _
            | DecisionTreeSuccess _
            | Delegate _
            // todo etc
            | _ ->
                false, acc
    snd (loop [] expr)
        
let filterFunctionMethodArgs (args: Ident list) =
    if args.Length = 1 && args.[0].Type = Unit then
        []
    elif args.Length = 2 && args.[0].IsThisArgument && args.[1].Type = Unit then
        [ args.[0] ]
    else args
//    args |> List.filter (fun i ->
//        match i.Type with
//        | Unit -> false
//        | _ -> true
//    )
let filterCallExprArgs (args: Expr list) =
    if args.Length = 1 && args.[0].Type = Unit then
        []
    // todo: how to check if args.[0] is this
    elif args.Length = 2 && true (* args.[0].IsThisArgument *) && args.[1].Type = Unit then
        [ args.[0] ]
    else args
//    args |> List.filter (fun i ->
//        match i.Type with
//        | Unit -> false
//        | _ -> true
//    )
let genericConstructorMacro (ent: Entity) (c: ClassDecl) =
    let constructorName = Print.constructorName c
    let genericArgs = ent.GenericParameters |> List.map (fun g -> g.Name)
    let constructorArgs = (c.Constructor.Value.Args |> filterFunctionMethodArgs |> List.map (fun i -> i.Name))
    let args = System.String.Join(", ", genericArgs @ constructorArgs)
    let transformedArgs = System.String.Join(", ", (genericArgs |> List.map(fun a -> $"TYPE_INFO({a})")) @ constructorArgs)
    $"#define {constructorName}({args}) {constructorName}____fable_compiler_impl({transformedArgs})"
let resolveType (generics: (string * Type) list) (t: Type) =
    match t with
    | GenericParam(name, isMeasure, constraints) ->
        match generics |> List.tryFind (fun (paramName, _) -> name = paramName) with
        | Some (_, t) -> t
        | _ -> t
    | _ -> t
let tryResolveType (generics: (string * Type) list) (t: Type) : Type option =
    match t with
    | GenericParam(name, isMeasure, constraints) ->
        generics |> List.tryFind (fun (paramName, _) -> name = paramName) |> Option.map snd
    | _ -> Some t
module Query =
    let exprIsDefaultOf (expr: Expr) =
        match expr with
        | Call (Import(importInfo, ``type``, sourceLocationOption), callInfo, _, _) ->
            match (importInfo.Selector, (Array.last (importInfo.Path.Split(char "/")))) with
            | "defaultOf", "Util.js" -> true
            | "defaultOf", "Util.c" -> true
            | selector, file when (List.contains "new" callInfo.Tags) -> 
                true
            | _ -> false
        | _ -> false
        
    let hasJsDependency (com: Type.ICompiler) (expr: Expr) =
        let typResult =
            match expr.Type with
            | DeclaredType(entityRef, genericArgs) ->
                let ent = com.TryGetEntity entityRef
                []
            | MetaType -> []
            | _ -> []
        match expr with
        | Call(callee, callInfo, ``type``, sourceLocationOption) ->
            let memberRef = callInfo.MemberRef
            let _member = memberRef |> Option.bind com.TryGetMember
            let ent = _member |> Option.map (fun m -> m.DeclaringEntity |> Option.map com.TryGetEntity)
            match callee with
            | Import(importInfo, ``type``, sourceLocationOption) ->
                []
            | Get(expr, kind, ``type``, sourceLocationOption) ->
                []
            | _ ->
                []
        | Import(importInfo, ``type``, sourceLocationOption) ->
            if importInfo.Path = "./fable_modules/fable-library-c/Fable.Core.JS.Console.js" then
                [ "./fable_modules/fable-library-c/Fable.Core.JS.Console.js" ]
            else
                []
        | _ ->
            []
            
    let grabDependencies (com: Type.ICompiler) (expr: Expr) =
        let mutable results = []
        let callback (e: Expr) =
            results <- results @ hasJsDependency com e
            None
        Fable.Transforms.AST.visitFromOutsideIn callback expr


    let rec typeDependencies (t: Type) =
        match t with
        | DeclaredType(entityRef, genericArgs) ->
            // todo: how do i solve this better so i stop getting issues sorting struct typedefs in the header
            if entityRef.FullName.Contains "nativeptr`1" ||
               entityRef.FullName.Contains "System.NativeArray`1" ||
               entityRef.FullName.Contains "System.Array`1" then []
            else [ entityRef.FullName ]
        | Array(genericArg, arrayKind) -> typeDependencies genericArg
        | DelegateType(argTypes, returnType) ->
            (argTypes |> List.map typeDependencies |> List.collect id)
            @ (typeDependencies returnType)
        | AnonymousRecordType(fieldNames, genericArgs, isStruct) ->
            genericArgs |> List.map typeDependencies |> List.collect id
        // todo
        | LambdaType(argType, returnType) -> []
        | _ -> []
    let classDeclDependencies (com: MyCompiler) (c: ClassDecl) : string list =
        let ent: Entity  = com.GetEntity(c.Entity.FullName)
        let deps = [
            for field in ent.FSharpFields do
                yield! typeDependencies field.FieldType
        ]
        deps |> List.distinct
    let structDirectDependencies (fields: C.Type list) =
        let rec loop f : string list =
            match f with
            | C.UserDefined(fullName, isValueType, entityOption) ->
                [ fullName ]
            | C.Ptr _type -> loop _type
            | C.FunctionPtr(types, returnType) ->
                let argsDependencies = types |> List.map loop |> List.collect id
                let returnTypeDependsOn = loop returnType
                argsDependencies @ returnTypeDependsOn
            | _ -> []
        fields |> List.map loop |> List.collect id |> List.distinct
                
    // todo: Taken from Fable2Rust.fs
//    let isClosedOverIdent (ident: Ident) =
//        not (ident.IsCompilerGenerated && ident.Name = "matchValue")
//        && not (ident.IsThisArgument) // todo: && ctx.IsClassMember)
//        // todo:
////        && (ident.IsMutable ||
////            isValueScoped ctx ident.Name ||
////            isRefScoped ctx ident.Name ||
////            shouldBeCloned com ctx ident.Type)
//    let getIgnoredNames (name: string option) (args: Fable.Ident list) =
//        let argNames = args |> List.map (fun arg -> arg.Name)
//        let allNames = name |> Option.fold (fun xs x -> x :: xs) argNames
//        allNames |> Set.ofList
//    let tryFindClosedOverIdent (ignoredNames: System.Collections.Generic.HashSet<string>) (expr: Expr) =
//        match expr with
//        | Fable.IdentExpr ident ->
//            if not (ignoredNames.Contains(ident.Name))
//                && (isClosedOverIdent ident)
//            then Some ident
//            else None
//        // add local names in the closure to the ignore list
//        // TODO: not perfect, local name shadowing will ignore captured names
//        | Fable.ForLoop(ident, _, _, _, _, _) ->
//            ignoredNames.Add(ident.Name) |> ignore
//            None
//        | Fable.Lambda(arg, _, _) ->
//            ignoredNames.Add(arg.Name) |> ignore
//            None
//        | Fable.Delegate(args, body, name, _) ->
//            args |> List.iter (fun arg ->
//                ignoredNames.Add(arg.Name) |> ignore)
//            None
//        | Fable.Let(ident, _, _) ->
//            ignoredNames.Add(ident.Name) |> ignore
//            None
//        | Fable.LetRec(bindings, _) ->
//            bindings |> List.iter (fun (ident, _) ->
//                ignoredNames.Add(ident.Name) |> ignore)
//            None
//        | Fable.DecisionTree(_, targets) ->
//            targets |> List.iter (fun (idents, _) ->
//                idents |> List.iter (fun ident ->
//                    ignoredNames.Add(ident.Name) |> ignore))
//            None
//        | _ ->
//            None
//    let hasCapturedNames (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
//        let ignoredNames = System.Collections.Generic.HashSet(getIgnoredNames name args)
//        let isClosedOver expr =
//            tryFindClosedOverIdent ignoredNames expr
//            |> Option.isSome
//        Fable.Transforms.FableTransforms.deepExists isClosedOver body
//    let isClosure (expr: Expr) =
//        match expr with
//        | Delegate(idents, body, stringOption, tags) ->
//            hasCapturedNames None idents body
////            let rec loop acc expr =
////                match expr with
////            let acc_idents = idents
////            false
//        | _ -> false
    let isEmptyDelegate (idents: Ident list) (body: Expr) =
        match body with
        | Call(callee, callInfo, ``type``, sourceLocationOption) ->
            let identsEqual (a: Ident) (b: Ident) =
                a.Name = b.Name && a.Type = b.Type
            let compareExprAndIdent = fun (arg: Expr) (ident: Ident) ->
                match arg with
                | IdentExpr i ->
                    let b = identsEqual i ident
                    if b = false then
                        ()
                    b
                | Value(UnitConstant, _) when ident.Type = Unit -> true
//                | Null _type -> true
                | _ -> false

            printfn $"{callInfo.Args.Length} = {idents.Length} && %A{callInfo.Args |> List.map (Print.printExpr 0)} \n %A{idents}"
            callInfo.Args.Length = idents.Length &&
            (List.map2 compareExprAndIdent callInfo.Args idents |> List.forall id)
            // true
        | _ -> false
    let emptyDelegate idents body =
        match isEmptyDelegate idents body, body with
        | true, Call (callee, info, typ, r) ->
            Some (callee, info, typ, r)
        | _ ->
            None
    let rec genericTypesUsedByType (generics: (string * Type) list) (com: MyCompiler) (t: Type) : GenericInteraction list =
        match t with
        | DeclaredType(entityRef, genericArgs) when not (entityRef.FullName.StartsWith("Microsoft.FSharp.Core")) && entityRef.FullName = "nativeptr`1" = false ->
            let unresolvedGenerics = generics |> List.map (snd >> (tryResolveType generics)) |> List.filter Option.isNone
            
            match com.TryGetEntity(entityRef.FullName) with
            | Some ent ->
                let fields = ent.FSharpFields |> List.map (fun f -> f.FieldType)
                if not (ent.FullName.StartsWith("Microsoft.FSharp.Core.PrintfFormat")) && unresolvedGenerics.Length > 0 then
                    (GenericInteraction.Instantiation (entityRef.FullName, List.map (resolveType generics) genericArgs))
                    :: List.collect (genericTypesUsedByType generics com) fields
                else []
            | None -> []
        | Array(genericArg, arrayKind) ->
            [ GenericInteraction.Instantiation ("System.Array`1", [ resolveType generics genericArg ]) ]
        | _ -> []
    let isUnresolvedGenericType: Type -> bool = function
        | GenericParam(_name, _isMeasure, _constraints) -> true
        | DeclaredType(_entityRef, genericArgs) ->
            // todo: this is triggered by this being byref<'t> for struct extension methods
            if _entityRef.FullName = "Microsoft.FSharp.Core.byref`2" then
                isUnresolvedGenericType genericArgs.[0]
            else
                genericArgs |> List.exists isUnresolvedGenericType
//                genericArgs.Length > 0
//            let genericParams = genericArgs |> List.filter (fun g -> match g with | GenericParam _ -> true | _ -> false)
//            genericParams.Length > 0
        | Array(genericArg, arrayKind) -> isUnresolvedGenericType genericArg
        // todo: why was this set to be true?
        | Any -> false
        | _ -> false
    let pullGenericTypes (args: Type list) : string list =
        let mutable items = []
        for t in args do
            match t with
            | Type.GenericParam(name, _isMeasure, _constraints) ->
                if not (items |> List.contains name) then
                    items <- name :: items
            | Type.DeclaredType (_entityRef, genericArgs) ->
                for g in genericArgs do
                    match g with
                    | GenericParam(name, _isMeasure, _constraints) ->
                        if not (items |> List.contains name) then
                            items <- name :: items
                    | _ ->
                        ()
            | _ ->
                ()
        items |> List.rev
    let find (f: Expr -> 't option) (expr: Expr) =
        let mutable returnValue = None
        expr |> Transforms.AST.visitFromOutsideIn (fun expr ->
            match f expr with
            | Some value -> 
                if returnValue = None then
                    returnValue <- Some (value, expr)
                Some expr
            | None -> None
        ) |> ignore
        returnValue
    /// Tests whether the given expression can be expressed as a single expression in C
    let rec isSimpleExpr (expr: Expr) =
        // let result =
        //     expr |> find (fun expr ->
        //         match expr with
        //         | Sequential _ | Let _ | WhileLoop _ | ForLoop _
        // //        | Delegate _
        //         | Lambda _
        //         | IfThenElse _ -> Some false
        //         | Value(StringTemplate _, _) -> Some false
        //         | _ -> Some true
        //     ) |> Option.bind fst |> Option.defaultValue false
            // |> Option.defaultValue (false, Unchecked.defaultof<_>) >> fst

        let oldResult =
            match expr with
            | Sequential _ | Let _ | WhileLoop _ | ForLoop _
    //        | Delegate _
            | Lambda _ -> false
            | IfThenElse (guard, thenExpr, elseExpr, _range) ->
                isSimpleExpr guard && isSimpleExpr thenExpr && isSimpleExpr elseExpr
            | Call(callee, callInfo, ``type``, sourceLocationOption) ->
                isSimpleExpr callee &&
                List.forall isSimpleExpr callInfo.Args &&
                (callInfo.ThisArg.IsNone || isSimpleExpr callInfo.ThisArg.Value)
            | Value(valueKind, _sourceLocationOption) ->
                match valueKind with
                | NewRecord(values, _entityRef, _genArgs) ->
                    values |> List.forall isSimpleExpr
                | StringTemplate(exprOption, parts, values) ->
                    values |> List.forall isSimpleExpr
                    //false
                | NewUnion (values, tag, entRef, genArgs) ->
                    let ent = database.contents.GetEntity(entRef)
                    if ent.IsValueType then List.forall isSimpleExpr values
                    else false
                | NewAnonymousRecord(values, fieldNames, genArgs, isStruct) ->
                    List.forall isSimpleExpr values
                | _ ->
                    true
            | Set(expr, kind, ``type``, value, sourceLocationOption) ->
                isSimpleExpr expr && isSimpleExpr value
            | DecisionTreeSuccess _ -> false
            | DecisionTree(expr, targets) -> false
            | TypeCast (expr, typ) -> isSimpleExpr expr
            | _ -> true 
        // if result <> oldResult then
        //     log $"{Print.printObj expr}"
        oldResult
        
    let isGenericDecl (memberDecl: MemberDecl) =
        let isGenericBody = isUnresolvedGenericType memberDecl.Body.Type
        let isGenericClass = memberDecl.Name.Contains "$"
        let hasGenericArgs =
            match memberDecl.MemberRef with
            | MemberRef(declaringEntity, memberRefInfo) ->
                match memberRefInfo.NonCurriedArgTypes with
                | Some args -> args |> List.exists isUnresolvedGenericType
                | _ -> false
            | _ ->
                false
        let memberDeclHasGenericArgs =
            memberDecl.Args |> List.exists (fun i -> isUnresolvedGenericType i.Type)
        isGenericBody || isGenericClass || hasGenericArgs || memberDeclHasGenericArgs
    
    /// Tests whether the expression must be expressed in a non-expression language (like C) as a series of statements
    let isComplexExpr = isSimpleExpr >> not

    let getIncludes (expr: Expr) =
        expr |> walkExpr (fun e ->
            match e with
            | Call(callee, callInfo, ``type``, sourceLocationOption) ->
                match callee with
                | IdentExpr ident ->
                    None
                | Import(importInfo, ``type``, sourceLocationOption)
                        when not (importInfo.Path.Contains "fable_modules" || importInfo.Path.Contains("src/fable-library-c/")) ->
                    // todo: Some <| importInfo.Path.Replace(".fs", ".h").Replace("./", "")
                    None
                | _ -> None
            | Import(importInfo, ``type``, sourceLocationOption)
                    when not (importInfo.Path.Contains "fable_modules") ->
                // todo: Some <| importInfo.Path.Replace(".fs", ".h")
                None
            | _ -> None
        ) |> List.filter Option.isSome |> List.map (Option.defaultValue "") |> List.distinct

let log (s: string) =
    let line =
        #if FABLE_COMPILER
        "[ Log ]"
        #else
        Environment.StackTrace.Split(char "\n").[2].Replace("   at ", "")
            .Replace("/home/dave/code/Fable", ".")
        #endif
    //printfn $"{line}:\n{s}\n"
    ()
    
type MemberDeclTransform = {
    memberDecl: MemberDecl
    m: MemberFunctionOrValue
}
type GenericUsage =
    | MethodCall of expr: Expr * callInfo: CallInfo * _type: Type * range: SourceLocation option
    
type GenericInteraction =
    | MethodCall of _member: MemberFunctionOrValue * fableMethodName: string * genericParams: Type list
    | Instantiation of fullName: string * genericParams: Type list
    | Tupl of genericParams: Type list * isStruct: bool
    | ValueOption of genericParam: Type
let inline duInfo (o: 't) =
    FSharp.Reflection.FSharpValue.GetUnionFields (o, o.GetType())
let pullGenericTypeUsages (com: Type.ICompiler) (t: Type) : GenericInteraction list =
    match t with
    | Array(genericArg, arrayKind) ->
        [ (GenericInteraction.Instantiation ("System.Array`1", [ genericArg ])) ]
    | DeclaredType(entityRef, genericArgs) when genericArgs.Length > 0 && not (entityRef.FullName.StartsWith "Microsoft.FSharp.Core.PrintfFormat") ->
        // todo: probably add the constructor
        [ (GenericInteraction.Instantiation (entityRef.FullName, genericArgs)) ]
    | Tuple(genericArgs, isStruct) ->
        [ Tupl (genericArgs, isStruct) ]
    | Option(genericArg, isStruct) ->
        [ ValueOption genericArg ]
    | _ -> []
let pullGenericTypes (com: Type.ICompiler) (args: Type list) : string list =
    let mutable items = []
    for t in args do
        match t with
        | Type.GenericParam(name, isMeasure, _constraints) ->
            if not (items |> List.contains name) then
                items <- name :: items
        | Type.DeclaredType (_entityRef, genericArgs) ->
            for g in genericArgs do
                match g with
                | GenericParam(name, isMeasure, _constraints) ->
                    if not (items |> List.contains name) then
                        items <- name :: items
                | _ ->
                    ()
        | _ ->
            ()
    items |> List.rev
// type Compiler =
//     abstract member TryGetEntity: EntityRef -> Entity option
//     abstract member TryGetMember: MemberRef -> MemberFunctionOrValue option
//     abstract member GetMember: MemberRef -> MemberFunctionOrValue
//     abstract member GetEntity: EntityRef -> Entity
//     abstract member ProjectFile: string
//     abstract member CurrentFile: string
// let com: ICompiler ref = ref Unchecked.defaultof<ICompiler>

module Type =
    open Fable.AST.Fable
    type ICompiler =
        abstract member TryGetEntity: EntityRef -> Entity option
        abstract member TryGetMember: MemberRef -> MemberFunctionOrValue option
        abstract member GetEntity: EntityRef -> Entity
        abstract member GetMember: MemberRef -> MemberFunctionOrValue
// let com: Fable.Compiler ref = ref Unchecked.defaultof<Fable.Compiler>
// let com: Type.ICompiler ref = ref Unchecked.defaultof<_>
let database: Type.ICompiler ref = ref Unchecked.defaultof<_>

let findGenerics (com: Type.ICompiler) (compiler: MyCompiler) generics expr : (GenericInteraction * Expr) list list =
    walkExpr (fun expr ->
        let fromType = pullGenericTypeUsages com expr.Type |> List.map (fun a -> a, Unchecked.defaultof<_>)
        let fromBody =
            match expr with
            | Call(IdentExpr ident, info, typ, range) 
                    when info.GenericArgs.Length > 0 ->
                match info.MemberRef |> Option.bind com.TryGetMember with
                | Some fsMember ->
                    [ ((GenericInteraction.MethodCall (fsMember, fsMember.CompiledName, info.GenericArgs |> List.map (resolveType generics))), expr) ]
                | None -> 
                    log $"Could not find member for ident call:\n {Print.printObj 0 info}\n{Print.printObj 0 expr}"
                    []
            | Call(Import(importInfo, import_type, import_source_location), callInfo, callType, callSourceLocation) ->
                match importInfo.Kind with
                | MemberImport (MemberRef(declaringEntity, memberRefInfo)) ->
                    try
                        let _member =
                            com.TryGetMember(MemberRef(declaringEntity, memberRefInfo))
                            
                            // todo
                            |> Option.defaultWith (fun () ->
                                    compiler.GetMember(declaringEntity.FullName.Split(char "`").[0] + "." +
                                            memberRefInfo.CompiledName
                                                .Replace("get_", "").Replace("set_", "")))
                        [ ((GenericInteraction.MethodCall (_member, memberRefInfo.CompiledName, callInfo.GenericArgs |> List.map (resolveType generics))), expr) ]
                    with ex ->
                        log $"{ex}"
                        for m in compiler.Members.Value.Values do
                            log $"{m.FullName}"
                        []
                    
                | ClassImport entityRef ->
                    // log $"%A{entityRef}"
                    []
                | LibraryImport libraryImportInfo ->
                    // log $"%A{importInfo}"
                    // log $"%A{libraryImportInfo}"
                    []
                // | UserImport isInline ->
                | _ ->
                    // log $"Not sure how to make use of import @ {if callSourceLocation.IsSome then string callSourceLocation.Value.start.line else (string null)} ({com.CurrentFile})"
                    // log <| ($"%A{Import(importInfo, import_type, import_source_location)} %A{callInfo} %A{callType}").Replace("\n", " ")
                    []
            | Call(callee, callInfo, ``type``, sourceLocationOption) ->
                let m = callInfo.MemberRef |> Option.map com.TryGetMember
                match m with
                | Some (Some m) ->
                    if callInfo.GenericArgs.Length > 0 then
                        let fableMethodName =
                            match callee with
                            | IdentExpr ident -> ident.Name
                            | Import(importInfo, ``type``, sourceLocationOption) ->
                                m.FullName
                                //m.DeclaringEntity.Value.FullName + importInfo.Selector
                            | _ -> ""
                        [ ((GenericInteraction.MethodCall (m, fableMethodName, callInfo.GenericArgs |> List.map (resolveType generics))), expr) ]
                    else
                        []
                | None when callInfo.GenericArgs.Length > 0 ->
                    log $"%A{expr}"
                    []
                | Some None
                | None ->
                    []
            | Value(valueKind, sourceLocationOption) ->
                match valueKind with
                | NewRecord(values, entityRef, genArgs) -> []
                | NewArray(newArrayKind, ``type``, arrayKind) -> []
                | _ -> []
            | Let(ident, value, body) ->
                let isGenericValue = Query.isUnresolvedGenericType value.Type
                //let body = (findGenericInstantiations depth value) @ (findGenericInstantiations (depth + 1) body)
                let identGenerics = pullGenericTypeUsages com ident.Type |> List.map (fun t -> t, expr)
                let valueGenerics =
                    if isGenericValue then
                        pullGenericTypeUsages com value.Type |> List.map (fun t -> t, expr)
                    else
                        []
                identGenerics @ valueGenerics
            | _ -> []
        fromType @ fromBody
    ) expr |> List.filter (fun l -> l.Length <> 0)
    
let rec findGenericInstantiations (depth: int) (expr: Expr) : (GenericInteraction * Expr) list =
    let o = FSharp.Reflection.FSharpValue.GetUnionFields (expr, expr.GetType())
//    for i in 1..depth do
//        printf "    "
//    printfn $"    | Fable.{(fst o).Name}"
    
    match expr with
    | Call(callee, callInfo, _type, sourceLocationOption) ->
        let m = callInfo.MemberRef |> Option.map database.contents.GetMember
        match m with
        | Some m ->
            if callInfo.GenericArgs.Length > 0 then
                let fableMethodName = match callee with | IdentExpr ident -> ident.Name | _ -> ""
                [ ((GenericInteraction.MethodCall (m, fableMethodName, callInfo.GenericArgs)), expr) ]
            else
                []
        | None ->
            []
    | Sequential exprs ->
        List.collect (findGenericInstantiations (depth + 1)) exprs
    | Let(ident, value, body) ->
        let isGenericValue = Query.isUnresolvedGenericType value.Type
        let body = (findGenericInstantiations depth value) @ (findGenericInstantiations (depth + 1) body)
        if isGenericValue then
            match value.Type with
            | DeclaredType(entityRef, genericArgs) ->
                (GenericInteraction.Instantiation (entityRef.FullName, genericArgs), value) :: body
            | _ ->
                body
        else
            body
    | _ ->
//        printfn "%A" expr
        []
module Display =
    let toString (g: GenericInteraction * Expr) =
        let (g, expr) = g
        match g with
        | MethodCall(memberFunctionOrValue, _, genericParams) ->
            $"MethodCall: {memberFunctionOrValue.FullName} {memberFunctionOrValue.DisplayName} %A{genericParams} ({(fst (duInfo expr)).Name})"
        | Instantiation(fullName, genericParms) ->
            $"Instantiation: {fullName} %A{genericParms} ({(fst (duInfo expr)).Name})"
    let genericInteraction (g: GenericInteraction) =
        match g with
        | Instantiation(fullName, genericParams) ->
            let s = genericParams |> List.map (fun p -> sprintf "%A" p)
            let s = String.Join(", ", s).Replace("\n", " ")
            $"Instantiation: {fullName} [{s}]"
        | MethodCall(memberFunctionOrValue, fableMethodName, genericParams) ->
            let s = genericParams |> List.map (fun p -> sprintf "%A" p)
            let s = String.Join(", ", s).Replace("\n", " ")
            $"MethodCall: {memberFunctionOrValue.DeclaringEntity.Value.FullName} {fableMethodName} [{s}]"
    let rec fableExpression (depth: int) (expr: Expr) = seq {
        let inline duInfo (o: 't) = FSharp.Reflection.FSharpValue.GetUnionFields(o, o.GetType())
        let (duInfo, duTupleValues) = duInfo expr
        let duTupleValuesText =
            let strings =
                duTupleValues |> Array.map (fun o -> sprintf "%A" o) |> Array.map (fun s -> $"{s}".Replace("\n", "|\\n|").Substring(0, Math.Min(s.Length, 40)))
            "(" + System.String.Join(", ", strings) + ")"
        for i in 1..depth do
            yield "    "
        match expr with
        | Let(ident, value, body) ->
            yield $"{duInfo.Name} {ident.Name} =\n"
            yield! fableExpression (depth + 1) value
            yield! fableExpression depth body
        | Sequential exprs ->
            yield $"{duInfo.Name}\n"
            for expr in exprs do
                yield! fableExpression (depth + 1) expr
        | _ ->
            yield $"{duInfo.Name} {duTupleValuesText}\n"
    }
    let memberDeclaration (com: Type.ICompiler) (memberDecl: MemberDecl) =
        let m = com.GetMember(memberDecl.MemberRef)
        printfn $"                                                                                       {memberDecl.Name} @ {m.FullName}"
        match memberDecl.MemberRef with
        | MemberRef(declaringEntity, memberRefInfo) ->
            let isGeneric = Query.isGenericDecl memberDecl
                
            let genericParamNames = memberRefInfo.NonCurriedArgTypes |> Option.map Query.pullGenericTypes
            
//            if not isGeneric then
            let compiledName =
                (m.FullName.Substring(0, m.FullName.LastIndexOf(".")) + "_" + memberDecl.Name)
                    .Replace(".", "_")
            printfn $"    compiledName = {compiledName}"
                
            if memberDecl.Name.Contains "cast" then
                ()
            printfn $"    isGeneric = {isGeneric}"
            if isGeneric then
                printfn $"    genericParamNames = {genericParamNames.Value}"
            if not isGeneric then
                ()
            match memberRefInfo.IsInstance with
            | true ->
                //printfn $"Transforming instance member ref: {declaringEntity.FullName} @ %A{memberRefInfo}"
                let lines = sprintf "%A" memberRefInfo
                let lines = lines.Split(char "\n") |> Array.map (fun s -> "    " + s)
                Console.WriteLine (System.String.Join("\n", lines))
            | false ->
                let lines = sprintf "%A" memberRefInfo
                let lines = lines.Split(char "\n") |> Array.map (fun s -> "    " + s)
                Console.WriteLine (System.String.Join("\n", lines))
            if not isGeneric then
                ()
    //            for s in statements do
    //                printfn $"%A{s}"
            else
                () // todo: add to generics
            // fableExpression 0 memberDecl.Body |> Seq.iter Console.Write
            //|> ignore
        | GeneratedMemberRef generatedMember ->
            ()
    //    [ sprintf "%A" memberDecl ]
        let result =
            findGenericInstantiations 1 memberDecl.Body
            |> List.map (fst >> genericInteraction)
            |> Set.ofSeq
        for i in result do
//                printf "\t\t\t\t\t\t\t\t"
            printfn $"{i}"
//            printfn $"{genericInteraction i}"
//            printfn $"%A{result |> List.map toString}"
        printfn "\n"
        { m = m; memberDecl = memberDecl }

type Print =
    static member inline repeat (i: int) (s: string) =
        let sb = StringBuilder()
        for i in 1..i do sb.Append(s) |> ignore
        sb.ToString()
    static member inline indent (depth: int) = Print.repeat (depth * 4) " "
    static member trim (s: string) =
        if s.Length = 0 || s.[0] <> char " " then
            s
        else
            let mutable count = 0
            let mutable whitespace = true
            while whitespace && count < s.Length do
                if s.[count] <> char " " then
                    whitespace <- false
//                    count <- count - 1
                count <- count + 1
    //        if whitespace then ("notrim " + s) else ($"trim {count} " + s.Substring(count))
            if whitespace then "" else s.Substring(count - 1)
        |> ignore
        s.TrimStart()
    #if FABLE_COMPILER
    static member printUnion (attribute: string) (depth: int) (o: obj) =
        sprintf "%A" o
    #else
    static member printUnion (attribute: string) (depth: int) (o: obj) =
        let (caseInfo, values) = FSharpValue.GetUnionFields(o, o.GetType())
        let valueStrings: string[] =
            (values, caseInfo.GetFields()) ||> Array.map2 (fun value f ->
                let fieldValue: string = Print.printObj (depth + 1) value
                $"{f.Name}= " + (Print.trim fieldValue)
            )
        let s: string = System.String.Join(",", valueStrings)
        if s.Length < 40 then
            (Print.indent depth) + $"{o.GetType().BaseType.Name}.{caseInfo.Name}{attribute} ({s})"
        else
            let inline repeat i (s: string) = System.String.Join(s, [ for i in 0..i do yield "" ])
            let sb = StringBuilder()
            sb.Append(repeat (depth * 4) " ").Append($"{o.GetType().BaseType.Name}.{caseInfo.Name}{attribute} (\n") |> ignore
            for v in valueStrings do
                sb.Append(repeat ((depth + 1) * 4) " ").Append($"{v},\n") |> ignore
            sb.Append(repeat (depth * 4) " ").Append(")").ToString()
    #endif
    static member printObjNoRecursion (depth: int) (o: obj) =
        if FSharpType.IsUnion(o.GetType()) then
            let name = o.GetType().Name
            if name = "FSharpList`1" then
                let _type = o.GetType().GenericTypeArguments.[0]
                let name = _type.Name
                let printList (list: obj) =
                    let rec loop (acc: string list) (node: obj) =
                        let (item, fields) = FSharpValue.GetUnionFields(node, o.GetType())
                        if item.Tag = 1 then
                            acc @ [ (Print.repeat ((depth + 1) * 4) " ") + ((Print.printObj (depth + 1) fields.[0]) |> Print.trim) ] @ (loop [] fields.[1])
                        else
                            acc
                    loop [] list
                let result = String.Join((Print.repeat ((depth + 1) * 4) " ") + "\n", printList o)
                (Print.indent depth) + $"(* List.{name} *) [\n{result}\n{Print.indent depth}]"
            else
                (Print.indent depth) + Print.printUnion "" depth o
                
        elif FSharpType.IsRecord(o.GetType()) then
            let fields = FSharpValue.GetRecordFields(o)
            let fieldNames = FSharpType.GetRecordFields(o.GetType()) |> Array.map (fun p -> p.Name)
            let namePairs = Array.map2 (fun field info -> $"{Print.indent (depth + 1)}{info} = {Print.printObj (depth + 1) field |> Print.trim}") fields fieldNames
            let s = System.String.Join(";\n", namePairs)
            (Print.indent depth) + $"{o.GetType().Name} {{\n{s}\n{Print.indent depth}}}"
        elif FSharpType.IsTuple(o.GetType()) then
            let fields = FSharpValue.GetTupleFields(o)
            let fieldStrings = fields |> Array.mapi (fun i f ->
                let item = (Print.printObj (depth + 1) f |> Print.trim)
                let item =
                    if item.Contains("\n") then
                        let lines = item.Split('\n')
                        String.Join("\n", [| (lines.[0] + $" (* Item {i} *)"); yield! (lines |> Array.skip 1) |])
                    else
                        item
                (Print.indent depth) + item)
            if (System.String.Join("", fieldStrings)).Length > 40 then
                let fieldNames = fields |> Array.map (fun f -> f.GetType().Name)
                let tupleText = String.Join(", ", fieldNames)
                $"{Print.indent depth} ({tupleText})\n" + System.String.Join("\n", fieldStrings)
            else
                (Print.indent depth) + System.String.Join(",", fields |> Array.map (Print.printObj depth))
        else
            o.ToString()
    static member printObj (depth: int) (o: obj) =
        try
            let result =
                match o with
                | :? Expr as expr ->
        //            let (C.Expr.Emit s) = Print.printComment o
                    Print.printExpr depth expr
        //            s
                | :? Option<SourceLocation> as location ->
                    match location with
                    | Some location -> $"Line {location.start.line}" // @ {location.start.column}"
                    | _ -> ""
                | :? Type as _type ->
                    match _type with
                    | DeclaredType(entityRef, genericArgs) -> entityRef.DisplayName
                    | _ -> $"%A{_type}".Replace("\n", "; ")
                | :? Ident as ident ->
                    $"\"{ident.Name}\": {Print.trim (Print.printObj depth ident.Type)}"
                | :? EntityPath as sourcePath ->
                    match sourcePath with | SourcePath s | AssemblyPath s | CoreAssemblyName s | PrecompiledLib (s, _) ->
                        s.Split(char "/") |> Array.last
                | :? MemberRef as memberRef ->
                    match memberRef with
                    | MemberRef(declaringEntity, memberRefInfo) ->
                        
                        let _member = database.contents.TryGetMember(memberRef) |> Option.map (fun m -> m.Attributes |> Seq.toArray |> Array.map (fun a -> a.Entity.FullName, a.ConstructorArgs))
                        $"%A{_member}"
                    | GeneratedMemberRef generatedMember ->
                        let declaredBy = generatedMember.Info.DeclaringEntity |> Option.map (fun d -> d.FullName) |> Option.defaultValue ""
                        $"{generatedMember.Info.Name}@{declaredBy}"
    //            | :? EmitInfo as emitInfo ->
    //                $"EmitInfo (\"{emitInfo.Macro}\", {Print.printObj 0 emitInfo.CallInfo}, IsStatement={emitInfo.IsStatement})"
    //            | :? CallInfo as callInfo ->
    //                let args = (callInfo.ThisArg |> Option.map (fun a -> [ a ]) |> Option.defaultValue []) @ callInfo.Args
    //                let args_string = System.String.Join(", ", args |> List.map (Print.printExpr 0))
    //                $"args=({args_string})"
                | _ ->
                    Print.printObjNoRecursion depth o
            let sb = StringBuilder()
    //        for i in 1..(depth * 4) do
    //            sb.Append(" ") |> ignore
            sb.Append(Print.indent depth).Append(result).ToString()
        with error -> $"{error}"
        
    static member printExpr depth (expr: Expr) : string =
        let rec loop depth (expr: Expr) : string =
            let sb = StringBuilder()
            match expr with
            | Let(ident, value, body) ->
                sb.Append (Print.indent depth) |> ignore
//                for i in 1..(depth * 4) do sb.Append(" ") |> ignore
//                if Query.isSimpleExpr value then
//                    sb.AppendLine $"let {ident.Name}: {Print.printObj 0 ident.Type} = \n{loop (depth + 1) value}\n{loop depth body}"
//                    |> ignore
//                else
                sb.AppendLine $"let {ident.Name}: {Print.printObj 0 ident.Type} =\n{loop (depth + 1) value}\n{loop depth body}"
                |> ignore
            | Sequential expressions ->
                for expr in expressions do
                    sb.AppendLine(loop depth expr) |> ignore
            | Import(importInfo, ``type``, sourceLocationOption) ->
                sb.Append $"Import (\"{importInfo.Selector}\", \"{importInfo.Path}\")" |> ignore
            | _ ->
//                sb.Append (Print.indent depth) |> ignore
//                let caseInfo, values = FSharpValue.GetUnionFields (expr, expr.GetType())
//                let valueStrings = System.String.Join(", ", values |> Array.map (Print.printObj 0))
//                sb.Append (Print.printObjNoRecursion 0 expr) |> ignore
                Print.printUnion
                    (if depth >= 1 then $" (* {(Print.printObjNoRecursion depth expr.Type |> Print.trim).Split(' ').[0]} *)" else "")
                    depth
                    expr
                |> (sb.Append >> ignore)
//                sb.Append($"{caseInfo.Name} ({valueStrings})") |> ignore
//                for value in values do
//                    sb.Append (Print.printObj depth value) |> ignore
//                sb.Append ")" |> ignore
//                    match value with
//                    | :? Option<SourceLocation> -> ()
//                    | :? Expr as e -> sb.Append (loop depth e) |> ignore
//                    | _ ->
//                        let formatted_string = $" %A{value} ".Replace("\n", "\\n ")
//                        sb.Append($" %A{value} ") |> ignore
            sb.ToString()
        let s = loop depth expr
//        let s = Print.printObj 0 expr.Type + s
        s
//        s.Replace("\n", "\n\n")
    static member printComment ([<ParamArray>] args: obj[]) : string =
        let mutable s = ""
        for arg in args do
            match arg with
            | :? Expr as expr ->
                s <- s + (Print.printExpr 0 expr)
            | _ ->
                s <- $"%A{arg}"
        // Environment.StackTrace + "\n" + s
        s
    static member emitComment (expr: obj) : C.Expr =
//        let mutable s = ""
//        for arg in args do
//            match arg with
//            | :? Expr as expr ->
//                s <- s + (Print.printExpr 0 expr)
//            | _ ->
//                s <- $"%A{arg}"
//        C.Expr.Emit $" /* %A{s} */"
        // todo: default value instead of (void*)0
        let _type =
            match expr with
            | :? Expr as expr ->
                match expr.Type with
                | Unit -> C.Void
                | _ -> C.Ptr C.Void
            | _ -> C.Ptr C.Void
        let value_text =
            match _type with
//            | C.Void -> "(void)(void*)0"
            | C.Ptr (C.Void) | _ -> "(void*)0"
        // C.Expr.Emit $"{value_text}" // /* %A{Print.printComment expr} */"
        C.Expr.Emit (Print.printComment expr)
    static member compiledFunctionSignature (name: string, args: (string * C.Type) list, return_type: C.Type) : string =
        let arg_string = System.String.Join(", ", args |> List.map (fun (name, t) -> $"{t.ToTypeString()}"))
        match return_type with
        | C.FunctionPtr(types, returnType) ->
            let fn_arg_string = System.String.Join(", ", types |> List.map (fun t -> $"{t.ToTypeString()}"))
            $"{returnType.ToTypeString()} (*func({arg_string}))({fn_arg_string});"
        | _ ->
            $"{return_type.ToTypeString()} {name}({arg_string});"
    static member compiledExternFunctionSignature (name: string, args: (string * C.Type) list, return_type: C.Type) : string =
        let arg_string = System.String.Join(", ", args |> List.map (fun (name, t) -> $"{t.ToTypeString()} {name}"))
        match return_type with
        | C.FunctionPtr(types, returnType) ->
            let fn_arg_string = System.String.Join(", ", types |> List.map (fun t -> $"{t.ToTypeString()}"))
            $"{returnType.ToTypeString()} (*func({arg_string}))({fn_arg_string});"
        | _ ->
            $"{return_type.ToTypeString()} {name}({arg_string});"
    static member compiledFunctionSignature (functionInfo: C.FunctionInfo) : string =
        Print.compiledFunctionSignature (functionInfo.id, functionInfo.args, functionInfo.return_type)
    static member compiledTypeName (_type: EntityRef) =
        _type.FullName.Replace(".", "_").Replace("Tmds_Linux_s", "")
    static member compiledTypeName (_type: MemberFunctionOrValue) =
        _type.FullName.Replace(".", "_").Replace("Tmds_Linux_s", "")
    static member compiledTypeName (generics: C.Type list, fullName: string) =
//        _type.GenericParameters
//        let generics = _type.GenericParameters |> List.zip generics
//        System.String.Join("_", generics |> List.map (transformType generics))
        let genericParamTypeString =
            if fullName.Contains "voidptr" then
                printfn "generic params for voidptr\n%A" generics
            System.String.Join("_", generics |> List.map (fun g -> g.ToNameString()))
        let className =
            fullName.Replace(".", "_")
                .Replace($"`{generics.Length}", $"__{genericParamTypeString}")
                .Replace($"${generics.Length}", $"__{genericParamTypeString}")
        className.Replace("Tmds_Linux", "")
    static member compiledTypeName (generics: C.Type list, _type: EntityRef) =
        Print.compiledTypeName (generics, _type.FullName)
    static member compiledMethodName (m: MemberDecl, generics: C.Type list, _type: EntityRef) =
        let ident =
            match m.MemberRef with
            | MemberRef(declaringEntity, memberRefInfo) -> memberRefInfo.CompiledName
            | _ -> ""
        Print.compiledMethodName (ident, generics, _type)
    static member compiledNamespace (e: EntityRef) =
        e.FullName.Replace(".", "_")
    static member constructorName (c: ClassDecl) =
        Print.compiledTypeName c.Entity + "_ctor"
    static member finalizerName (c: ClassDecl) =
        Print.compiledTypeName c.Entity + "_Finalizer"
    static member finalizerName (generics, fullName: string) =
        (Print.compiledTypeName (generics, fullName)) + "_Finalizer"
    static member finalizerName (generics, e: Entity) =
        (Print.compiledTypeName (generics, e.FullName)) + "_Finalizer"
    static member finalizerName (generics, e: EntityRef) =
        (Print.compiledTypeName (generics, e.FullName)) + "_Finalizer"
    static member compiledReturnType (generics: (string * Type) list, transformType: (string * Type) list -> Type -> C.Type) (com: Type.ICompiler) (m: MemberDecl) =
        if m.Name.Contains "ctor" then
            match m.MemberRef with
            | MemberRef(declaringEntity, memberRefInfo) ->
                C.Ptr (C.EmitType (Print.compiledTypeName(generics |> List.map (snd >> (transformType generics)), declaringEntity)))
                // todo
//                C.Ptr (C.UserDefined (Print.compiledTypeName(generics, declaringEntity), declaringEntity))
//                C.Ptr (C.EmitType "")
            | _ ->
                C.Type.EmitType $"%A{m}"
                // failwith "Shouldn't happen"
        else
            (m.Body.Type |> (transformType generics))
    static member argString (args: (string * C.Type) list) =
        Compiler.writeFunctionArgs args
    static member compiledMethodName (ident: string, generics: C.Type list, _type: Entity) =
        Print.compiledMethodName(ident, generics, _type)
    static member compiledMethodName (ident: string, generics: C.Type list, _type: EntityRef) =
        Print.compiledMethodName(ident, generics, _type.FullName)
    static member compiledMethodName (ident: string, generics: C.Type list, fullName: string) =
        let method =
            if generics.Length > 0 && not (fullName.Contains "$" || fullName.Contains "`") then
                let genericParamTypeString =
                    System.String.Join("_", generics |> List.map (fun g -> g.ToNameString()))
                fullName.Replace(".", "_") + "_" + ident + "__" + genericParamTypeString
            else
                let name = Print.compiledTypeName (generics, fullName)
                $"{name}_{ident}".Replace("$ctor", "ctor").Replace(".ctor", "ctor")
        method
        //if method.EndsWith "ctor" then $"&{method}" else method
    static member compiledMethodName (_member: MemberDecl, generics: C.Type list, _type: Entity) =
        let ident: string =
            match _member.MemberRef with
            | MemberRef(declaringEntity, memberRefInfo) -> memberRefInfo.CompiledName
            | _ -> ""
        Print.compiledMethodName(ident, generics, _type.FullName)
    static member compiledMethodName(generics: (string * Type) list, transformType: (string * Type) list -> Type -> C.Type, com: Type.ICompiler, m: MemberDecl) =
        let name =
            match m.MemberRef with
            | MemberRef(declaringEntity, memberRefInfo) ->
                memberRefInfo.CompiledName
            | _ ->
                m.Name
        Print.compiledMethodName (name, (List.map snd generics) |> List.map (transformType generics), com.GetMember(m.MemberRef).DeclaringEntity.Value)
    static member compiledTypeSignature(generics: (string * Type) list, transformType: (string * Type) list -> Type -> C.Type, com: Type.ICompiler, m: MemberDecl) =
        let name =
            match m.MemberRef with
            | MemberRef(declaringEntity, memberRefInfo) ->
                memberRefInfo.CompiledName
            | _ ->
                m.Name
        let method_name = Print.compiledMethodName (name, (List.map snd generics) |> List.map (transformType generics), com.GetMember(m.MemberRef).DeclaringEntity.Value)
        let return_type = Print.compiledReturnType (generics, transformType) com m
        let args =
            m.Args
            |> List.map (fun arg ->
                match isArgValueThis generics com arg with
                // | Some ident -> ident
                | _ -> arg)
            |> List.filter (fun arg -> match arg.Type with | Unit -> false | _ -> true)
            |> List.map (fun a -> (a.Name, transformType generics a.Type))
        let args = Print.argString args
        $"{return_type.ToTypeString()} {method_name}({args});"

let isArgValueThis generics (com: Type.ICompiler) (arg: Ident) : Ident option =
    if arg.IsThisArgument then
        match arg.Type with
        | DeclaredType(entityRef, genericArgs) when entityRef.FullName = "Microsoft.FSharp.Core.byref`2" && arg.IsThisArgument ->
            match (tryResolveType generics genericArgs.[0]) with
            | Some (DeclaredType(entityRef, _)) ->
                match com.TryGetEntity(entityRef) with
                | Some ent when ent.IsValueType -> 
                    //Some { arg with Name = "this_value"; Type = genericArgs.[0] }
                    None
                | _ -> None
            | _ -> None
        | _ -> None
    else None

let isCallToConstructor (value: Expr) =
    match value with
    | Call(Import({ Kind = MemberImport (MemberRef.MemberRef (memberRef, info)) }, _, _), _, _, _)
    | Call(IdentExpr _, { MemberRef = Some (MemberRef.MemberRef (memberRef, info)) }, _, _)
        when info.CompiledName = ".ctor" -> true
    | _ -> false

let isEntryPoint (com: Type.ICompiler) (m: MemberDecl) =
    let _member = com.TryGetMember(m.MemberRef)
    match _member with
    | Some _member ->
        let attributes = _member.Attributes |> Seq.toList
        attributes |> Seq.exists (fun a -> a.Entity.FullName = "Microsoft.FSharp.Core.EntryPointAttribute")
    | _ -> false
    
let typeFullName (t: Type) =
    match t with
    | DeclaredType(entityRef, genericArgs) -> entityRef.FullName
    | _ -> "" // todo
let unwrapLambda ident body =    
    let rec loop acc (body: Expr) : Ident list * Expr =
        match body with
        | Lambda(ident, body, stringOption) -> loop (acc @ [ ident ]) body
        | t -> acc, body
    loop [ ident ] body
