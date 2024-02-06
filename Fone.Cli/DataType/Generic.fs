module Fone.DataType.Generic

open System.Text

open System
open System.Collections

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.C.Helpers
open Fable.Transforms.FSharp2Fable

open Fable.C
open Helpers
open Transforms
open Fone.DataType.State

type debug =
    static member inline log ([<ParamArray>] o:  obj[]) = Console.WriteLine $"[DEBUG]: %A{o}"

let writeMethod context (memberFunctionOrValue: MemberFunctionOrValue) fableMethodName genericParams usage (generic_implementations: Generic.List<_>) (added_functions: Generic.List<_>) (generic_interactions: byref<_>) =
    let declaringEntity = memberFunctionOrValue.DeclaringEntity
    match declaringEntity with
    | Some declaringEntity ->
        let ent: Entity option =
            compiler.TryGetEntity(declaringEntity.FullName)
        match ent with
        | None ->
            log $"======================= missing entity =============================="
            #if !FABLE_COMPILER
            log $"{Print.printObj 0 declaringEntity}"
            log $"{fableMethodName} - {Print.printObj 0 genericParams}"
            #endif
            let paramNames =
                match snd usage with
                | Call (_, info, _, _) ->
                    info.SignatureArgTypes
                    |> List.map (fun g ->
                        match g with
                        | GenericParam (name, _, _) -> Some name | _ -> None)
                    |> List.filter (Option.isSome)
                    |> List.map (Option.defaultValue "")
                | _ ->
                    log $"======================= todo handle non call generic member decl entity =============================="
                    []
            let generics = genericParams |> List.zip paramNames
            for kv in genericMethodDeclarations.contents do
                ()
                // log $"%A{kv.Key} - %A{kv.Value.MemberRef}"
            // todo: should it be added to the dict with member ref full name?
            let methodDeclKey = (declaringEntity.FullName, fableMethodName)
            if genericMethodDeclarations.contents.ContainsKey(methodDeclKey) then
                let decl = genericMethodDeclarations.contents.[(declaringEntity.FullName, fableMethodName)]
                let (function_id, type_sig, function_info) = Function.Generics.addMethodImplementation context generics declaringEntity.FullName true genericParams fableMethodName decl
                generic_implementations.Add(type_sig, Writer.writeFunction (SourceBuilder()) function_info)
                added_functions.Add(function_id)
            else
                log $"Unable to find key for entity ({declaringEntity.FullName}, {fableMethodName})"
        | Some ent ->
            //log $"======================= found entity =============================="
            let entityFullName = ent.FullName //.Split("`").[0]
            let otherName = $"{ent.DisplayName}_{fableMethodName}"
            if (genericMethodDeclarations.contents.ContainsKey (entityFullName, fableMethodName) ||
               genericMethodDeclarations.contents.ContainsKey (entityFullName, otherName)) then
                let member_declaration =
                    Map.tryFind (ent.FullName, fableMethodName) genericMethodDeclarations.contents
                    |> Option.defaultWith (fun () -> genericMethodDeclarations.contents.[(entityFullName,
                                                                                          otherName)])
    //                                      pullGenericTypes <|
    //                                      (ent.GenericParameters) @
    //                                      (member_declaration.Args |> List.map (fun a -> a.Type)) @
    //                                      [ member_declaration.Body.Type ]
                let genericArgNames =
                    (ent.GenericParameters @ memberFunctionOrValue.GenericParameters)
                    |> List.map (fun p -> p.Name)
                    |> List.distinct
                let generics = (genericParams |> List.zip genericArgNames)


                let typeName = Print.compiledTypeName(List.map (transformType generics) genericParams, declaringEntity)
                //let args = (member_declaration.Args |> List.map (fun i -> i.Name, transformType generics i.Type))
                let (function_id, type_sig, function_info) = Function.Generics.addMethodImplementation context generics ent.FullName ent.IsValueType genericParams fableMethodName member_declaration
                // todo: Do closures defined in generic methods work already? Or is this needed
                // let toCompile = Function.gatherAnonymousFunctions context member_declaration.Body
                // if toCompile.Length > 0 then
                //     ()
                if not (added_functions.Contains(function_id)) then
                    generic_implementations.Add(type_sig, Writer.writeFunction (SourceBuilder()) function_info)
                    added_functions.Add(function_id)
                    let generic_instantiations =
                        ent.FSharpFields
                        |> List.collect (fun f -> Query.genericTypesUsedByType generics compiler f.FieldType)
                    let generic_instantiations =
                        (generic_instantiations @ (List.collect (Query.genericTypesUsedByType generics compiler) (List.map (_.Type) member_declaration.Args)))
                        |> List.map (fun i -> i, Unchecked.defaultof<_>)
                    if generic_instantiations.Length > 0 then
                        generic_interactions <- generic_instantiations @ generic_interactions
                    let generic_instantiations = findGenerics database.contents compiler generics member_declaration.Body |> List.collect id
                    if generic_instantiations.Length > 0 then
                        generic_interactions <- generic_instantiations @ generic_interactions
                //let type_sig = Print.compiledTypeSignature (generics, transformType, database.contents, member_declaration)
                if fableMethodName.Contains "ctor" then
                    let finalizer_id = $"{typeName}_Destructor"
                    if not (added_functions.Contains(finalizer_id)) then
                        added_functions.Add(finalizer_id)
                        let finalizer_implementation = Function.buildFinalizer generics genericParams ent
                        generic_implementations.Add(($"void {typeName}_Destructor({typeName} this$);"), (Writer.writeFunction (SourceBuilder()) finalizer_implementation))
            else
                // todo WHAT THE FUCK D: <
                ()
    | None ->
        log $"=========================== missing entity for method ===================================="
        log memberFunctionOrValue.FullName

// todo: We shouldn't pass in a StringBuilder to modify
let findGenericInteractionsFromNonGenerics (path: string) (projHeaderName: string) =
    // Find the generic interactions
    log $"Finding generic interactions from non-generic declarations for file {path}"
    let mutable generic_interactions = [
        (GenericInteraction.Instantiation ("System.Array`1", [ Type.String ])), Unchecked.defaultof<_>
        (GenericInteraction.Instantiation ("System.Array`1", [ Type.Char ])), Unchecked.defaultof<_>
    ]
    for kv in initDeclarations.Value do
        let generic_instantiations =
            // Pulls generic types from function body
            (fst kv.Value) |> List.map (fun a -> findGenerics database.contents compiler [] a.Body) |> List.collect id |> List.collect id
        if generic_instantiations.Length > 0 then
            generic_interactions <- generic_instantiations @ generic_interactions
    let compilerFiles = compiler.Files |> Array.ofSeq
    for file in compilerFiles do
        let compilationResult = compiler.GetResult(file)
//        for kv in memberDeclarations.Value do
        for kv in compilationResult.memberDeclarations do
            let decl, compiledFunction = kv.Value
            let generic_instantiations =
                // Pulls generic types from function body
                (findGenerics database.contents compiler [] decl.Body |> List.collect id)
                @
                // Pulls generic types from function args
                ((List.collect (Query.genericTypesUsedByType [] compiler) (List.map (_.Type) decl.Args)) |> List.map (fun a -> a, Unchecked.defaultof<_>))
            // log $"====================== find results ========================"
            // log $"\n{Print.printObj 0 generic_instantiations}"
            if generic_instantiations.Length > 0 then
                generic_interactions <- generic_instantiations @ generic_interactions
//        for kv in classDeclarations.Value do
        for kv in compilationResult.classDeclarations do
            let generic_instantiations =
                let t = DeclaredType (kv.Value.Entity, [])
                Query.genericTypesUsedByType [] compiler t
                |> List.map (fun i -> i, Fable.Value (ValueKind.UnitConstant, None))
            generic_interactions <- generic_instantiations @ generic_interactions
    generic_interactions


let generateGenericImplementations context path projHeaderName : string * Generic.List<string * string> =
    printfn "Generating generic implementations"
    let declarations = CompiledOutputBuilder()
    let sb = StringBuilder()
    for kv in compiler.genericClassDeclarations.Value do
        log $"Creating declare macro for {kv.Value.Entity.FullName}"
        // todo: breaking with GcArray (see other comment)
        // sb.AppendLine $"{(Generics.macroForGenericClass kv.Value)}"
        // debug.log $"{(Generics.macroForGenericClass kv.Value)}"
        ()

    // todo: these are picking up things like
        // type List<'t>() =
        //    let mutable size = 2
        //    let mutable count = 0
        //    let items: 't[] = createArray  (sizeof<'t> * 2)
//    for kv in genericMethodDeclarations.Value do
//        let generic_instantiations = findGenerics kv.Value.Body |> List.collect id
//        if generic_instantiations.Length > 0 then
//            generic_classes_used <- generic_instantiations @ generic_classes_used
//            ()
//    for kv in genericClassDeclarations.Value do
//        match kv.Value.Constructor with
//        | Some constructor ->
//            let generic_instantiations = findGenerics constructor.Body |> List.collect id
//            if generic_instantiations.Length > 0 then
//                generic_classes_used <- generic_instantiations @ generic_classes_used
//                ()
//        | _ -> ()
    let _ = () //generic_classes_used |> List.map fst |> List.choose

    let mutable generic_interactions = findGenericInteractionsFromNonGenerics path projHeaderName
    generic_interactions <- generic_interactions @ closureTypes.Value
    closureTypes.Value <- []

    let added_types = Generic.List<string>()
    let added_functions = Generic.List<string>()
    let generic_implementations = Generic.List<_>()
    let generic_macro_defs_to_call = Generic.List<_>()
    let tuples = Generic.List<_>()
    let valueOptions = Generic.List()
    debug.log $"{DateTime.Now.ToLongTimeString()} Monomorphization step"
    // Reduce set of interactions to contain no duplicates, then iterate thru
    // them adding additionally found generic interactions to generic_interactions along the way
    while generic_interactions.Length > 0 do
        let temp =
            generic_interactions
            |> List.filter (fun (i, _) ->
                match i with
                | Instantiation(_, genericParams) -> List.forall (fun p -> match p with | GenericParam _ -> false | _ -> true) genericParams
                | MethodCall(_, _, genericParams) -> List.forall (fun p -> match p with | GenericParam _ -> false | _ -> true) genericParams
                | Func _ | Closure _ -> true
                | ValueOption _ | Tupl _ -> true
            )
            |> List.distinctBy (fun (i,_) ->
                try
                    match i with
                    | GenericInteraction.Instantiation(fullName, genericParams) ->
                        fullName + (System.String.Join(", ", genericParams |> List.map (fun p -> (transformType [] p).ToNameString())))
                    | GenericInteraction.MethodCall(memberFunctionOrValue, fableMethodName, genericParams) ->
                        memberFunctionOrValue.FullName + fableMethodName + (System.String.Join(", ", genericParams |> List.map (fun p -> (transformType [] p).ToNameString())))
                    | GenericInteraction.Tupl (args, isStruct) ->
                        tupleName [] args
                    | GenericInteraction.ValueOption arg ->
                        (transformType [] arg).ToTypeString()
                    | GenericInteraction.Closure (captured, args, _return) ->
                        Print.closureTypeName (transformType [], captured, args, _return)
                    | GenericInteraction.Func (args, _return) ->
                        Print.funcTypeName (transformType [], args, _return)
                with ex ->
                    ""
            )
        generic_interactions <- []
        for usage in temp do
            printfn $"Generic: %A{usage}"
            match fst usage with
            | Closure(captured, types, ``return``) ->
                let info = Func.writeClosure captured types ``return``
                generic_implementations.Add(info.decl, info.code)
            | Func(types, ``return``) ->
                let info = Func.write types ``return``
                generic_implementations.Add(info.decl, info.code)
            | Tupl (args, is_struct) ->
                tuples.Add args
            | ValueOption arg ->
                valueOptions.Add arg
            | Instantiation (fullName, genericParms) when fullName = "System.Array`1" ->
                let result = Core.writeArray genericParms[0]
                if added_types.Contains result.name then
                    ()
                else
                    added_types.Add result.name
                    generic_implementations.Add(result.decl, result.code)
            | Instantiation(fullName, genericParams) when
                    compiler.genericClassDeclarations.Value.ContainsKey fullName
                    || compiler.genericClassDeclarations.Value.ContainsKey $"Fable.{fullName}"
                    || fullName.StartsWith("System.Array") ->
                let usePolyfillFromMixin =
                    compiler.genericClassDeclarations.Value.ContainsKey $"Fable.{fullName}"
                let implName = if usePolyfillFromMixin then "Fable." + fullName else fullName
                let classDecl = compiler.genericClassDeclarations.Value.[if fullName = "System.Array" then "System.Array`1" else implName]
//                let ent = database.contents.GetEntity(classDecl.Entity)
                let ent = compiler.GetEntity implName
                let genericArgNames =
                    ent.GenericParameters |> List.map (fun p -> p.Name)
                let generics = (genericParams |> List.zip genericArgNames)
                let name = Print.compiledTypeName((List.map (transformType generics) genericParams), classDecl.Entity)
                if not (added_types.Contains(name)) then
                    let methods =
                        genericMethodDeclarations.Value
                        |> Map.filter (fun k v -> fst k = implName)
                        |> Map.toList |> List.map snd
                        |> List.map (fun m ->
                            try
                                let asdf =
                                    database.contents.TryGetMember(m.MemberRef)
                                    |> Option.defaultWith (fun () ->
                                        match m.MemberRef with
                                        | MemberRef(declaringEntity, memberRefInfo) ->
                                            for m in compiler.Members.Value do
                                                debug.log("%s", m.Value.FullName)
                                            try compiler.GetMember(declaringEntity.FullName.Split(char "`").[0] + "." + memberRefInfo.CompiledName.Replace("get_", "").Replace("set_", ""))
                                            with ex ->
                                                debug.log $"{ex}"
                                                Unchecked.defaultof<_>
                                        | _ -> Unchecked.defaultof<_>
                                    )
                                Some <| (GenericInteraction.MethodCall (asdf, m.Name, genericParams), Unchecked.defaultof<_>)
                            with ex -> raise ex)
                        |> List.filter Option.isSome
                        |> List.map Option.get
                    generic_interactions <- methods @ generic_interactions
                    let fields = ent.FSharpFields
                    let generic_types =
                        (List.map (transformType generics) genericParams)
                    let name = Print.compiledTypeName(generic_types, classDecl.Entity)
                    let compiledFields =
                        fields |> List.map (fun f -> (transformType generics f.FieldType, f.Name))
                    let genericTypesUsedByFields =
                        fields |> List.map (fun f -> Query.genericTypesUsedByType generics compiler f.FieldType) |> List.collect id |> List.map (fun f -> f, Unchecked.defaultof<_>)
                    generic_interactions <- genericTypesUsedByFields @ generic_interactions
    //                // todo: add any generic types as a required def
    //                for field in fields do
    //                    if field.FieldType.Generics.Length > 0 then
    //                        ()
    //                    ()
    //                context.Value <- addToModule name (C.Struct { tag = name; members = compiledFields }) context.Value
                    let names =
                        generic_types
                        |> List.map (fun t -> t.ToNameString())
                        |> fun c_names -> String.Join(", ", c_names)
                    let className = classDecl.Entity.FullName.Replace(".", "_").Split('`').[0]
                    sb.AppendLine($"// Declare_{className}({names});") |> ignore

                    if ent.IsFSharpUnion then
                        let info = (ent |> Core.writeUnionToBuilder genericParams declarations)
                        generic_implementations.Add(info.decl, info.code)
                    else
                        let ctor =
                            database.contents.TryGetMemberByRef (ent.FullName, ".ctor")
                            |> Option.map (fun (ent, file, mem, fsMember) -> mem)
                        declarations.AppendLine <| Core.writeStruct generics ent ctor (name, compiledFields |> List.map (fun (_a, _b) -> (_b,_a))) // <3
                        |> ignore
                    added_types.Add(name)

                    let constructor_name = name + "_ctor"
                    if not ent.IsValueType && not ent.IsFSharpUnion then
                        let finalizer_id = $"{name}_Destructor"
                        if not (added_functions.Contains(finalizer_id)) then
                            added_functions.Add(finalizer_id)
                            let finalizer_implementation = Function.buildFinalizer generics genericParams ent
                            generic_implementations.Add(($"void {finalizer_id}(struct {name}* this$);"), (Writer.writeFunction (SourceBuilder()) finalizer_implementation))
                    if classDecl.Constructor.IsSome then
                        let generic_instantiations =
                            fields
                            |> List.collect (fun f -> Query.genericTypesUsedByType generics compiler f.FieldType)
                            |> List.map (fun i -> i, Unchecked.defaultof<_>)
                        if generic_instantiations.Length > 0 then
                            generic_interactions <- generic_instantiations @ generic_interactions
                        let (id, type_sig, function_info) = Function.buildConstructor context generics genericParams classDecl.Constructor.Value ent.FullName ent.IsValueType
                        if not (added_functions.Contains(id)) then
                            added_functions.Add(id)
        //                    sb.AppendLine (Compiler.writeFunction (SourceBuilder()) function_info) |> ignore
                            generic_implementations.Add((type_sig, Writer.writeFunction (SourceBuilder()) function_info))
            | Instantiation(fullName, genericParams) when not (compiler.genericClassDeclarations.Value.ContainsKey fullName) ->
                let generic_param_text_types = genericParams |> List.map (fun t -> (transformType [] t).ToTypeString())
                generic_macro_defs_to_call.Add((fullName, generic_param_text_types))
            | Instantiation(fullName, types) ->
                log $"============================ instantiation ======================================"
                #if !FABLE_COMPILER
                log $"{fullName}:\n {Print.printObj 1 types}"
                #endif
                () // System.Array
            | MethodCall(memberFunctionOrValue, fableMethodName, genericParams) ->
                writeMethod context memberFunctionOrValue fableMethodName genericParams usage generic_implementations added_functions &generic_interactions
    for (entityFullName, generic_text_types) in generic_macro_defs_to_call do
        if not (entityFullName.StartsWith("Microsoft.FSharp.Core.PrintfFormat")) &&
           not (entityFullName.StartsWith("Microsoft.FSharp.Core.byref")) &&
           not (entityFullName.Contains "nativeptr") then
            let fullName = entityFullName.Replace($"`{generic_text_types.Length}", "").Replace(".", "_")
            let args_text = System.String.Join(", ", generic_text_types)
            // todo: was breaking on an empty project with Declare_System_GcArray
            // sb.AppendLine $"Declare_{fullName}({args_text})" |> ignore
            ()
    // todo: These also need to be a part of the topological sorting
    for tuple in tuples do
        let tplName = tupleName [] tuple
        declarations.AppendLine $"typedef struct {tplName} {{" |> ignore
        // todo: Only add __refcount for non-struct tuples
        declarations.AppendLine "    unsigned char __refcount;" |> ignore
        tuple |> List.iteri (fun index t -> declarations.AppendLine $"    {(transformType [] t).ToTypeString()} value_{index};" |> ignore)
        declarations.AppendLine $"}} {tplName};" |> ignore
        let destructorCalls =
            [|
            for i in 0..tuple.Length - 1 do
                let t = tuple[i]
                if requiresTracking [] t then
                    let c_type = transformType [] t
                    let c_type = c_type.ToNameString()
                    let c_type = c_type.Substring(0, c_type.Length - 3)
                    $"Runtime_end_var_scope(this$->value_{i}, {c_type}_Destructor);"
            |]
            |> String.concat "\n    "
        let destructorSig = $"void {tplName}_Destructor({tplName}* this$)"
        declarations.AppendLine $"{destructorSig};" |> ignore
        let impl =
            $"
{destructorSig} {{
    {destructorCalls}
    free(this$);
}}"
        let typeDecl = $"void {tplName}_Destructor({tplName}* this$);"
        declarations.AppendLine typeDecl |> ignore
        let values =
            tuple
            |> List.map (transformType [])
            |> List.map _.ToTypeString()
        let namedValues =
            values
            |> List.mapi (fun index name -> name + " value_" + string index)
        let declArgs = values |> String.concat ", "
        let ctor_calls =
            [|
                for i in 0..tuple.Length - 1 do
                    let t = tuple[i]
                    let c_type = transformType [] t
                    $"this$->value_{i} = value_{i};"
                    if requiresTracking [] t then
                        let c_type = c_type.ToNameString()
                        let c_type = c_type.Substring(0, c_type.Length - 3)
                        $"this$->value_{i}->__refcount++;"
            |]
            |> String.concat "\n    "
        let implArgs = namedValues |> String.concat ", "
        declarations.AppendLine $"struct {tplName}* {tplName}_ctor({declArgs});" |> ignore
        let ctor_impl = $"
struct {tplName}* {tplName}_ctor({implArgs}) {{
    struct {tplName}* this$ = malloc(sizeof(struct {tplName}));
    this$->__refcount = 1;
    {ctor_calls}
    return Runtime_autorelease(this$, {tplName}_Destructor);
}}"
        generic_implementations.Add ("", ctor_impl)
        generic_implementations.Add ("", impl)
    // Trying to sort by the name of ValueOption instances to help with declaration order
    for option in (valueOptions |> Seq.sortBy (fun o -> (transformType [] o).ToTypeString().Split("ValueOption".ToCharArray()).Length)) do
        let t = transformType [] option
        let typeName = t.ToNameString()
        let name = "ValueOption_" + typeName
        declarations.AppendLine $"typedef struct {name} {{" |> ignore
        declarations.AppendLine $"    int tag;" |> ignore
        declarations.AppendLine $"    {t.ToTypeString()} value;" |> ignore
        declarations.AppendLine $"}} {name};" |> ignore
    for (decl, _) in generic_implementations do
        declarations.AppendLine(decl)
        |> ignore
    declarations.ToString(), generic_implementations
