module rec Fone.DataType.Function

open System
open Fable.AST.Fable
open Fable.C.Transforms
open Fable.C.Helpers
open Fable.C.AST
open Fable.C
open Fable.C.C99Compiler
open Fable.AST

module print =
    let printfn _ = ()

let buildConstructor context generics genericParams (member_declaration: MemberDecl) (fullName: string) isValueType =
    print.printfn $"Build constructor {fullName}"
    let f = transformMember context generics member_declaration.Body
    let args = (member_declaration.Args |> List.map (fun i -> i.Name, transformType generics i.Type))
    let c_types = List.map snd generics |> List.map (transformType generics)
    let id = Print.compiledMethodName("ctor", c_types, fullName)
    let typeName = Print.compiledTypeName(List.map (transformType generics) genericParams, fullName)
    let type_sig = Print.compiledTypeSignature (generics, transformType, context.db, member_declaration)
    let finalizer_name = Print.finalizerName (c_types, fullName)
    let return_type = if isValueType then C.UserDefined (typeName, true, None) else C.Ptr (C.UserDefined (typeName, true, None))
    let function_info: C.FunctionInfo = {
        id = id
        return_type = return_type
        args = args
        body = [
            C.Declaration {
                name = "this$"
                // todo: can't change this from C.EmitType to C.UserDefined because it would cause a destructor
                    // maybe we can give constructors memory instead of allocating it here, or somehow denote that this isn't Gc'd since it's returned?
                    // maybe that means we have an op called runtime_escapes_scope to tell the GC that someone will take over or something idk
                    // or we could pass in a ref to a variable to put the obj into instead of declaring it here
//                    value = C.ExprAssignment <| C.TypeCast (C.EmitType $"{typeName}*", (C.Expr.Emit $"Runtime_gc_malloc(sizeof({typeName}), &{finalizer_name})"))
                value =
                    if isValueType then
                        C.Default
                    else
                        C.ExprAssignment <| C.TypeCast (C.EmitType $"{typeName}*", (C.Expr.Emit $"calloc(1, sizeof({typeName}))"))
                _type = return_type
                requiresTracking = false
            }
            // todo: Runtime_track_var
            // C.Statement.Expression <|
            //     C.Call ("Runtime_track_var", [ C.Expr.Emit "(void**)&this$" ])
//                if not (ent.IsValueType) then
//                    // todo: re-enable this
//                    C.Statement.Emit $"Runtime_set_finalizer(this$, &{finalizer_name});"
            C.Emit "__thread_context++;"
            if not isValueType then
                C.Emit $"this$->__refcount = 1;"
            // todo: Initialize variables
            yield! f
            C.Statement.Emit ("// use_gc_for_address()")
            // todo: This should actually return ident, but is this ok for now?
            // C.Emit $"this$->__refcount = this$->__refcount - 1;"
            // todo: This should use autorelease
            // todo: Do constructors need to use thread context?
            C.Emit "__thread_context--;"
            if not isValueType then
                C.Return (C.Expr.Emit $"Runtime_autorelease(this$, {finalizer_name})")
            else
                C.Return (C.Expr.Emit $"this$")
        ]
    }
    (id, type_sig, function_info)

let buildFinalizer generics genericParams (ent: Entity) =
    let name = Print.compiledTypeName((List.map (transformType generics) genericParams), ent.FullName)
    let fields = ent.FSharpFields
    let finalizer_id = $"{name}_Destructor"
    {
        C.FunctionInfo.id = finalizer_id
        C.FunctionInfo.return_type = C.Void
        C.FunctionInfo.body = [
            if ent.MembersFunctionsAndValues |> Seq.exists (fun m -> m.CompiledName.EndsWith "_Free") then
                C.Emit $"{name}__Free(this$);"
            // todo: instead of calling free, reach out ot the runtime so it can call any finalizers
            for field in fields do
                match field.FieldType with
                | DeclaredType(entityRef, genericArgs) ->
                    match database.contents.TryGetEntity(entityRef) with
                    | Some e ->
                        if not (e.IsValueType) then
                            // let c_generics = genericArgs |> List.map (transformType generics)
                            // C.Emit $"{Print.finalizerName (c_generics, e)}(this$->{field.Name});"
                            let fieldTypeName = transformType generics field.FieldType |> _.ToNameString()
                            C.Emit $"Runtime_end_var_scope(this$->{field.Name}, {fieldTypeName}_Destructor);"
                    | None when entityRef.FullName = "nativeptr`1" -> ()
                    | _ -> ()
//                            C.Emit $"free(this$->{field.Name});"
                | Array(genericArg, arrayKind) ->
                    let fieldTypeName = transformType generics genericArg |> _.ToNameString()
                    C.Emit $"Runtime_end_var_scope(this$->{field.Name}, System_Array__{fieldTypeName}_Destructor);"
                    // C.Emit $"free(this$->{field.Name});"
                | _ -> ()
            if not ent.IsValueType then
                C.Emit "free(this$);"
        ]
        C.FunctionInfo.args = [
            if not ent.IsValueType then
                ("this$", C.Ptr (C.UserDefined (name, false, None)))
            else
                ("this$", C.UserDefined (name, false, None))
        ] // Some ent))) ]
    }
let transformFunc context (name: string) (args: Ident list) (funcBody: Expr) (generics: (string * Type) list) : C.FunctionInfo =
    // (Unchecked.defaultof<Fable.Compiler>).GetImplementationFile
    // todo:
    // let args = args |> List.filter (fun ident -> ident.Type <> Fable.Unit)
    let context = { context with idents = (args |> List.map _.Name) @ context.idents }
    let addReturn (generics: (string * Type) list) (expr: Expr) : C.Statement list =
        let rec loop (generics: (string * Type) list) (body: C.Statement list) : C.Statement list =
            let last =
                // match (if (requiresTracking generics expr.Type) then body[body.Length - 2] else List.last body) with
                // match (if body.Length > 1 then body[body.Length - 2] else List.last body) with
                match List.last body with
                | C.Expression cExpr ->
                    [
                        // if body.Length > 1 then
                            // C.Emit "// __thread_context--;"
                            // ()
                        // C.Emit "// __thread_context--;"
                        // todo: If the return type requires tracking, then it needs to be tracked with autorelease
                        // ex: returning obj.aString while later obj is unassigned which would clean up aString too early
                        if requiresTracking generics expr.Type then
                            // todo: Assign to toReturn
                            // C.Return (C.Expr.Emit $"Runtime_autorelease({Compiler.writeExpression cExpr})")
                            (C.Emit $"__toReturn = {Writer.writeExpression cExpr};")
                            (C.Emit "__toReturn->__refcount++;")
                        else
                            (C.Emit $"__toReturn = {Writer.writeExpression cExpr};")
                            // C.Return (C.Expr.Emit (Compiler.writeExpression cExpr))
                    ]
                | C.Conditional(guard, ifTrue, ifFalse) ->
                    let ifTrue = loop generics ifTrue
                    let ifFalse = loop generics ifFalse
                    [ C.Conditional (guard, ifTrue, ifFalse) ]
                | _ -> failwith "Shouldn't happen"
            (body |> List.take (body.Length - 1)) @ last
        let body = transformMember context generics expr
        if expr.Type = Type.Unit then // || body.Length = 1 then
            [
                C.Emit "__thread_context++;"
                yield! body
                C.Emit "__thread_context--;"
                C.Emit "Runtime_clear_pool();"
                C.Return (C.Expr.Emit "UNIT")
            ]
    //    elif body.Length = 1 then
    //        [
    //            C.Statement. (body.[0])
    //        ]
        else
            [
                // if requiresTracking generics expr.Type then
                C.Emit $"{(transformType generics funcBody.Type).ToTypeString()} __toReturn;"
                C.Emit "__thread_context++;"
                if body.Length > 1 then
                    () // C.Emit "// __thread_context++;"
                yield! loop generics body
                C.Emit "__thread_context--;"
                C.Emit "Runtime_clear_pool();"
                if requiresTracking generics expr.Type then
                    match transformType generics funcBody.Type with
                    | C.Ptr typ ->
                        C.Return (C.Expr.Emit $"Runtime_autorelease(__toReturn, {typ.ToNameString()}_Destructor)")
                    | _else ->
                        ()
                else
                    C.Return (C.Expr.Emit "__toReturn")
            ]
    let dir = IO.Path.GetDirectoryName(context.currentFile)
    let filename = IO.Path.GetFileName(context.currentFile)
    #if !FABLE_COMPILER
    let result = Print.printExpr 1 funcBody
    let debug_dir = IO.Path.Join(dir, "fone")
    if not (IO.Directory.Exists(debug_dir)) then
        IO.Directory.CreateDirectory(debug_dir)
        |> ignore
    try
        Console.WriteLine $"Writing AST file to {debug_dir} {name}.debug.fs"
        // IO.File.WriteAllText (IO.Path.Join(debug_dir, name + ".debug.fs"), result)
        io.file.write (IO.Path.Join(debug_dir, name + ".debug.fs"), result)
    with
        error -> printfn $"{error}"
//    print.printfn $"{result}"

    // todo: no io in transforms!
    // if not (IO.Directory.Exists(io.path.join(dir, "build"))) then
    //     IO.Directory.CreateDirectory(io.path.join(dir, "build"))
    //     |> ignore

    let result = result.Replace("\n\n\n", "")
    io.file.AppendAllText(io.path.join(dir, $"build/{filename}.debug.fs"), $"let {name}_original () =\n{result}\n\n")
    io.file.AppendAllText(io.path.join(dir, $"build/{filename}.{name}.debug.fs"), $"let {name} () =\n{result}\n\n")
    #endif

    let (args, extra_statements) = transformMemberDeclArgs generics args
//    let (args, extra_statements) = args, []
    let rt =
        match funcBody.Type with
        // | Unit -> C.Void
        | _ -> transformType generics funcBody.Type
    let body = addReturn generics funcBody

    print.printfn $"[Fable.NativeCode] Transforming function: {name}"
//        let result = Print.printExpr 1 (applyTransformations funcBody)
//        let result = result.Replace("\n\n\n", "")
//    print.printfn $"{result}"
//    let body =
//        if body.Length > 1 then [
//            C.Emit "__thread_context++;"
//            yield! body
//            C.Emit "__thread_context--;"
//        ] else
//            body
//    let body =
////        let body = transformMember generics funcBody
//        if funcBody.Type <> Type.Unit && body.Length > 0 then
//            let last = body[if body.Length = 1 then 0 else body.Length - 2]
//            match last with
//            | C.Expression expr ->
//                if body.Length > 1 then
//                    body[0..(body.Length - 3)] @ [ body.[body.Length - 1] ] @ [ C.Return expr ]
//                else
//                    // todo: i think there's still some cases where we need to update __thread_context (ex: the return is a call to something else)
//                        // todo: actually i don't think it would matter in that case since there's no variables to assign to in this context
//                    [
////                        C.Emit "__thread_context++;"
////                        C.Declaration { _type = rt; name = "to_return"; value = C.ExprAssignment expr }
////                        C.Emit "__thread_context--;"
//                        C.Return expr
//                    ]
//            | C.Conditional(guard, ifTrue, ifFalse) ->
//                C.Conditional(guard, )
//            | _ ->
//                []
////                        failwith "should've been an expr as the last item"
//        else
//            body
    let args =
        args
        |> filterFunctionMethodArgs
        |> List.map (fun a -> (a.Name, transformType generics a.Type))
    {
        id = name
        return_type = rt
        args = args
        body = extra_statements @ body
    }
let transformDecl context generics (memberDecl: MemberDecl) =
    transformFunc context memberDecl.Name memberDecl.Args memberDecl.Body generics

module Type =
// old code when constructor declaration was inline transformFile
//        match c.Value.Constructor with
//        | Some constructor ->
//            let f = buildConstructor transformType transformMember c.Value constructor
//            let s = Compiler.writeFunction (SourceBuilder()) f
//            _sb.AppendLine(s) |> ignore
//        | _ -> ()
//        memberDeclarations += ((classDecl.Entity.FullName, Print.constructorName classDecl), constructor)

//        Compiler.write
//        context.Value <- addToModule name (C.Struct { tag = name; members = compiledFields }) context.Value
//        addConstructor context classDecl
//        sb.AppendLine $"{c.Key}" |> ignore
    let transformDeclaration context (com: Type.ICompiler) (generics: (string * Type) list) (classDecl: ClassDecl) : _ list * Map<_,_> =
        let entityRef = classDecl.Entity
        match com.TryGetEntity(classDecl.Entity) with
        | None ->
            print.printfn $"\n\nCouldn't find entity {classDecl.Entity}"
            [], Map.empty
        | Some ent ->
            let ent = com.GetEntity(classDecl.Entity)
            let emitTypeAttr = ent.Attributes |> Seq.tryFind (fun attribute -> attribute.Entity.FullName.Contains("EmitType")) // = Const.emitType)
            match emitTypeAttr with
            | Some attribute ->
                ([], Map.empty)
            | None ->
                let mutable includes = []
                let compiledModule = ref Map.empty
                let isGeneric = generics.Length > 0
                match classDecl.Constructor with
                | Some constructor ->
                    includes <- Query.getIncludes constructor.Body @ includes
                    let sb = SourceBuilder()
                    let ent = com.GetEntity(classDecl.Entity)
                    // Filter out types that inherit from System.Attribute
                    if ent.BaseType |> Option.map (fun t -> t.Entity.FullName) <> Some "System.Attribute" then
                        let finalizer = Function.buildFinalizer [] [] (com.GetEntity(classDecl.Entity))

                        print.printfn $"Compiling function {finalizer.id}"
        //                print.printfn $"{Compiler.writeFunction sb f}"

                        compiledModule += (finalizer.id, C.Function finalizer)
                        let (name, type_signature, f) =
                            Function.buildConstructor context [] [] constructor ent.FullName ent.IsValueType

                        print.printfn $"Compiling function {f.id}"
        //                print.printfn $"{Compiler.writeFunction sb f}"

                        compiledModule += (name, C.Function f)
        //                classDeclarations += (classDecl.Entity.FullName, classDecl)
                        compiler.UpdateFile(context.currentFile, FileCompilationResults.AddClassDeclaration, classDecl)
                    match context.db.TryGetEntity(classDecl.Entity) with
                    | Some ent -> compiler.AddEntity(ent)
                    | _ -> ()
                    #if !FABLE_COMPILER
                    if displayExpressions then
                        Display.memberDeclaration com constructor |> ignore
                    #endif
                | None ->
                    if ent.IsFSharpRecord && not ent.IsValueType then
                        // let (name, type_signature, f) =
                        //     Function.buildConstructor context [] [] constructor ent.FullName ent.IsValueType
                        let typeName = Print.compiledTypeName ([], ent.FullName)
                        let name = $"{typeName}_ctor"

                        let return_type =
                            if ent.IsValueType then
                                C.UserDefined (typeName, true, None)
                            else C.Ptr (C.UserDefined (typeName, true, None))
                        let argType = C.UserDefined (typeName, true, None)
                        let f: C.FunctionInfo = {
                            id = name
                            return_type = return_type
                            args = [ ("data", argType) ]
                            body = [
                                C.Emit $"{typeName}* this$ = malloc(sizeof({typeName}));"
                                C.Emit $"*this$ = data;"
                                C.Return (C.Call ("Runtime_autorelease", [ (C.Expr.Emit "this$"); C.Expr.Emit $"{typeName}_Destructor" ]))
                            ]
                        }
                        compiledModule += (name, C.Function f)
                    if not isGeneric then
    //                    classDeclarations += (classDecl.Entity.FullName, classDecl)
                        let ent = com.GetEntity(classDecl.Entity)
                        let f = Function.buildFinalizer [] [] ent
                        compiledModule += (f.id, C.Function f)
                        // compiler.UpdateFile(context.currentFile, FileCompilationResults.AddClassDeclaration, classDecl)
                (includes, compiledModule.Value)
let gatherUnfoundIdents (existingIdents: string list) (e: Expr) =
    let mutable idents = existingIdents
    let mutable captured = []
    let fn (expr: Expr) =
        match expr with
        | Let(ident, value, body) ->
            idents <- ident.Name :: idents
            expr
        | IdentExpr ident ->
            if not (List.contains ident.Name idents) then
                captured <- ident :: captured
            expr
        | expr -> expr
    Fable.Transforms.AST.visit fn e |> ignore
    captured
let gatherAnonymousFunctions2 existingIdents (expr: Expr) =
    let mutable previousIdents: Ident list = existingIdents
    let mutable acc = []
    let fn e =
        match e with
        | Let(ident, value, body) ->
            previousIdents <- ident :: previousIdents
            None
        | Delegate(idents, body, stringOption, tags) ->
            let args, body = idents, body
            let captured =
                Query.identsCaptured (previousIdents |> List.map _.Name) e
                |> Seq.map (fun kv -> let ident = kv.Value in ident)
                |> Seq.toList
            acc <- ((captured @ args), e, body) :: acc
            let more = gatherAnonymousFunctions2 previousIdents body
            acc <- more @ acc
            Some e
        | Lambda(ident, body, stringOption) ->
            let args, body = unwrapLambda ident body
            let captured =
                Query.identsCaptured (previousIdents |> List.map _.Name) e
                |> Seq.map (fun kv -> let ident = kv.Value in ident)
                |> Seq.toList
            acc <- ((captured @ args), e, body) :: acc
            let more = gatherAnonymousFunctions2 previousIdents body
            acc <- more @ acc
            Some e
        | CurriedApply(applied, exprs, ``type``, sourceLocationOption) ->
            let argTypes, returnType = unwrapLambdaType applied.Type
            if exprs.Length = argTypes.Length then None
            else
                let captured = applied :: exprs
                let remainingArgs = [
                    for i in 0..(argTypes.Length - exprs.Length) - 1 do
                        {
                            Name = $"arg_{i}"
                            IsMutable = false
                            Type = argTypes[exprs.Length + i]
                            IsThisArgument = false
                            IsCompilerGenerated = false
                            Range = e.Range
                        }
                ]
                let allArgs = captured @ (remainingArgs |> List.map IdentExpr)
                let anonymousFnArgs = [
                    for i in 0..allArgs.Length - 1 do
                        {
                            Name = $"arg_{i}"
                            IsMutable = false
                            Type = allArgs[i].Type
                            IsThisArgument = false
                            IsCompilerGenerated = true
                            Range = allArgs[i].Range
                        }
                ]
                // let fnArgs =
                //     (captured |> List.map (fun c ->
                //         match c with
                //         | Ident ident -> ident
                //         |
                //     ))
                //     @ remainingArgs
                let info: CallInfo = {
                    ThisArg = None
                    Args = (List.tail anonymousFnArgs) |> List.map IdentExpr
                    // todo: SignatureArg
                    SignatureArgTypes = argTypes
                    GenericArgs = []
                    MemberRef = None
                    Tags = []
                }
                acc <- (anonymousFnArgs, e, Fable.Call(IdentExpr anonymousFnArgs[0], info, returnType, e.Range)) :: acc
                // todo: let more = ... previousIdents applied?
                None
        | e ->
            None
    Fable.Transforms.AST.visitFromOutsideIn fn expr
    acc
let gatherAnonymousFunctions (existingIdents: Ident list) (context: Context) (body: Expr) : (string * C.ModuleDeclaration) list =
    let anonymous_functions =
        Function.findAnonymousFunctions context body
        |> List.choose id
    let mutable toCompile = []
    for fn in anonymous_functions do
        match fn with
        | (Delegate(idents, delegateBody, stringOption, tags), delegateIdents) ->
            let (delegateExpr, _) = fn
            let capturedIdents = gatherUnfoundIdents (idents |> List.map _.Name) delegateBody
            let idents = capturedIdents @ idents
            // if Query.isClosure context.idents delegateBody then
            //     let id = getAnonymousFunctionName context context.currentFile delegateExpr
            //     let capturedIdents =
            //         Query.identsCaptured context.idents delegateBody
            //         |> Seq.map (fun kv -> {
            //             Name = kv.Key
            //             Type = kv.Value.Type
            //             IsMutable = false
            //             IsThisArgument = false
            //             IsCompilerGenerated = false
            //             Range = kv.Value.Range
            //         })
            //         |> List.ofSeq
            //     let f = Function.transformFunc context id (capturedIdents @ idents) delegateBody []
            //     toCompile <- toCompile @ [
            //         id,
            //         C.Function f
            //     ]
            // else
            do
                let isSimple = Query.isEmptyDelegate idents delegateBody
                if not isSimple then
                    let id = getAnonymousFunctionName context context.currentFile delegateExpr
                    let f = Function.transformFunc context id idents delegateBody []
    //                            if Query.isClosure (fst fn.Value) then
                    let isIdentUsed name expr =
                        printfn "TODO: isIdentUsed for identifying closures in the compiler"
                        false
                    match delegateIdents |> List.tryFind (fun i -> isIdentUsed i.Name delegateBody) with
                    | Some i ->
                        printfn "Can't handle closures"
                    | _ ->
                        toCompile <- toCompile @ [ (id, C.Function f) ]
                else ()
        | _ -> ()
    let anon2 = gatherAnonymousFunctions2 existingIdents (Sequential [ body ])
    // todo: shadowing original
    let mutable toCompile = []
    for (idents, declExpr, functionExpr) in anon2 do
        let id = getAnonymousFunctionName context context.currentFile functionExpr
        let id = getAnonymousFunctionName context context.currentFile declExpr
        let rec loop idents expr =
            match idents with
            | [] -> expr
            | ident::idents -> loop idents (expr |> replaceMutableValueCapturedIdent context ident)
        let functionExpr =
            let identsToReplace = idents |> List.filter (fun ident -> ident.IsMutable && Query.requiresRefCell context [] ident.Type)
            loop identsToReplace functionExpr
        let idents = idents |> List.map (fun ident ->
            if ident.IsMutable && Query.requiresRefCell context [] ident.Type then
                match context.db.TryGetEntity "Fable.Ref`1" with
                | Some (ent, file) ->
                    { ident with Type = DeclaredType (ent.Ref, [ ident.Type ]) }
                | _else ->
                    failwith $"%A{_else}"
            else ident
        )
        let f = Function.transformFunc context id idents functionExpr []
        toCompile <- toCompile @ [ (id, C.Function f) ]
    toCompile |> List.distinctBy fst
let findAnonymousFunctions ctx (expr: Expr) : Option<(Expr * Ident list)> list =
    let mutable childDelegates = []
    let f (e: Expr) =
        match e, e.Range with
        | Delegate(idents, body, stringOption, tags), Some range ->
            Some (e, gatherIdentsBefore e expr)
        | Delegate _, None ->
            Some (e, gatherIdentsBefore e expr)
//                None
        | Lambda(ident, body, stringOption), _ ->
            let uncurriedArgs, unwrappedLambdaBody = unwrapLambda ident body
            if Query.isEmptyDelegate uncurriedArgs unwrappedLambdaBody then
                let rec loop e =
                    match e with
                    | Lambda (ident, body, _) ->
                        childDelegates <- e :: childDelegates
                        loop body
                    | _ -> ()
                loop e
                None
            elif (List.contains e childDelegates) = false then
                printfn "Adding delegate!"
                printfn $"{Print.printExpr 0 e}"
                printfn $"Existing delegates: %A{childDelegates |> List.map (Print.printExpr 2)}"
                Some (Delegate(uncurriedArgs, unwrappedLambdaBody, None, []), gatherIdentsBefore e expr)
            else None
        | _ -> None
    expr |> walkExpr (fun e ->
//                let f = Function.transformFunc id idents delegateBody []
//                            if Query.isClosure (fst fn.Value) then
//                match delegateIdents |> List.tryFind (fun i -> Fable.Transforms.FableTransforms.isIdentUsed i.Name delegateBody) with
//                | Some i ->
//                    print.printfn "Can't handle closures"
//                | _ ->
//                    compiledModule += (id, C.Function f)
        match e with
        | Fable.Let(ident, value, body) ->
            match f value with
            | Some (lambdaExpr, idents) ->
                let value =
                    match value with
                    | Fable.Lambda(lambdaIdent, body, stringOption) ->
                        let visit = Fable.Transforms.AST.visit
                        let name = getAnonymousFunctionName ctx ctx.currentFile lambdaExpr
                        let args, body = unwrapLambda lambdaIdent body
                        let rec replace (e: Expr) =
                            match e with
                            | Fable.Call (Fable.IdentExpr i, _info, _typ, _range) when i.Name = ident.Name ->
                                Fable.Call (Fable.IdentExpr { i with Name = name }, _info, _typ, _range)
                            | Fable.CurriedApply(Fable.IdentExpr i, exprs, ``type``, sourceLocationOption)
                                    when i.Name = ident.Name  ->
                                Fable.CurriedApply(Fable.IdentExpr { i with Name = name }, exprs, ``type``, sourceLocationOption)
                            | e -> visit replace e
                        let delegateExpr = body |> replace
                        Delegate(args, delegateExpr, None, [])
                        // if not (Query.isClosure (List.tail ctx.idents) body) then
                        //     // body |> walkExprInPlace replace
                        // else
                        //     value
                    | value -> value
                Some (value, idents)
            | None -> None
        | _ -> None
    )
module Generics =
    let getGenericParams (c: ClassDecl) : string list =
        let ent = compiler.TryGetEntity(c.Entity.FullName)
        ent |> Option.map (fun ent -> ent.GenericParameters |> List.map (fun p -> p.Name))
        |> Option.defaultValue []
    let getFields (c: ClassDecl) : (string * C.Type) list =
        // let ent =
        compiler.TryGetEntity(c.Entity.FullName)
        |> Option.map (fun ent -> ent.FSharpFields |> List.map (fun f -> f.Name, transformType [] f.FieldType))
        |> Option.defaultValue []
    let macroForGenericClass (c: ClassDecl) : string =
        let macro_name = "Declare_" + c.Entity.FullName.Split(char "`").[0].Replace(".", "_")
        let macroArgs = getGenericParams c
        let macro_args_text = System.String.Join(", ", macroArgs)
        let fields = getFields c
        let name_with_args_added = c.Entity.FullName.Replace(".", "_").Replace($"`{macroArgs.Length}", "__" + System.String.Join("_", macroArgs))
        $"""#define {macro_name}({macro_args_text}) typedef struct {name_with_args_added} {{\
    {System.String.Join(";\\\n    ", fields |> List.map Writer.writeStructField) + ";\\\n"}\
    }} {name_with_args_added};"""
    let addMethodImplementation context (generics: (string * Type) list) (entityName: string) (isValueType: bool) (genericParams: Type list) (fableMethodName: string) (member_declaration: MemberDecl) : string * string * C.FunctionInfo =
        if fableMethodName.Contains "ctor" then
            Function.buildConstructor context generics genericParams member_declaration entityName isValueType
        else
            //let name = Print.compiledMethodName(fableMethodName, c_types, declaringEntity)
            let type_sig = Print.compiledTypeSignature (generics, transformType, context.db, member_declaration)
            let name = Print.compiledMethodName (generics, transformType, context.db, member_declaration)
            if name.EndsWith("Counter_AddItem") then
                Transforms.debugger.contents <- true
            let function_info = Function.transformFunc context name member_declaration.Args member_declaration.Body generics
            Transforms.debugger.contents <- false
            (name, type_sig, function_info)
// #if !INTERACTIVE
// [<EntryPoint>]
// #endif
