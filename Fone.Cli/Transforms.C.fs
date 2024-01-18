module rec Fable.C.Transforms

open System
open Fable.AST.Fable
open Fable.C.Helpers
open Fable
open Fable.AST
open Fable.C.AST
open Fable.Transforms.FSharp2Fable
open Microsoft.FSharp.Reflection
open Fable.C.C99Compiler
open ReplaceExpr

let inline debug _ _ = ignore

let displayExpressions = false
let compiler = MyCompiler()

let includedHeaders = ref []
let includedPackages = ref []
// let conditional_packages = ref Map.empty
let header_emit_texts = ref []
let isByRefType (t: Fable.Type) =
    match t with
    | Fable.DeclaredType(e, genericArgs) ->
        match e.FullName with
        | "Microsoft.FSharp.Core.byref`2" | "Microsoft.FSharp.Core.ByRefKinds.InOut" ->
            true
        | _ ->
            false
    | _ ->
        // todo
        false

// let isTransforming = ref false

type Flags =
    static member UseBlockExprForConditionalGuards = false

// todo: emittifyStringTemplate vs evaluate
let rec emitEvaluateStringTemplate ctx (com: Type.ICompiler) (generics: (string * Type) list) (parts: string list) (values: Expr list) : string =
    let rec evaluateValue (value: Fable.Expr) =
        match value with
        | Fable.Value(valueKind, sourceLocationOption) ->
            match valueKind with
            | Fable.StringConstant value ->
                // todo: ad sanitize string function
                // "\"" + value.Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r") + "\""
                // value.Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r")
                value
            | Fable.NumberConstant(value, numberKind, numberInfo) -> string value
            | Fable.BoolConstant value -> string value
            | Fable.StringTemplate(exprOption, parts, values) -> emitEvaluateStringTemplate ctx com generics parts values
            | Fable.Null _ -> "NULL"
            | Fable.TypeInfo (typ, tags) ->
                match typ with
                | Fable.DeclaredType (entityRef, genericArgs) ->
                    let isEmitAttribute (attr: Fable.Attribute) =
                        attr.Entity.FullName.Contains("EmitType")
                    match com.TryGetEntity(entityRef) with
                    | Some ent ->
                        match ent.Attributes |> Seq.tryFind isEmitAttribute with
                        | Some attr -> string attr.ConstructorArgs.[0]
                        | _ -> Print.compiledTypeName (genericArgs |> List.map (transformType generics), entityRef.FullName)
                    | _ -> (transformType generics typ).ToTypeString()
                | _ -> (transformType generics typ).ToTypeString()
            | Fable.UnitConstant -> "()"
            | _ -> $"%A{value}" // todo: arrays and unions and records n such
        | Fable.IdentExpr ident ->
            let idents = compiler.GetIdents()
            match idents |> List.tryFind (fun (i, _) -> i.Name = ident.Name) with
            // todo: we should only evaluateValue if we can?
//            | Some (i, value) -> evaluateValue value
            // todo: probably a source of bugs
            | _ -> ident.Name
        | Fable.Operation(operationKind, [], ``type``, sourceLocationOption) ->
            Compiler.writeExpression (transformOperation ctx generics operationKind ``type``)
        | Fable.TypeCast(expr, ``type``) -> evaluateValue expr
        | Fable.Emit(emitInfo, ``type``, sourceLocationOption) ->
            let result = transformExpr ctx generics value
            Compiler.writeExpression result
        | Fable.Lambda(arg, body, name) ->
            let args, body = unwrapLambda arg body
            if Query.isEmptyDelegate args body then
                let (Call (callee, callInfo, _, _)) = body
                match callInfo.MemberRef with
                | Some (Fable.MemberRef (entRef, memInfo)) ->
                    $"""{entRef.FullName.Replace(".", "_")}_{memInfo.CompiledName}"""
                    // evaluateValue callee
                | _ ->
                    evaluateValue callee
            else
                Print.printComment value
        | _ ->
            Print.printComment value

    let mutable s = ""
    for i in 0..(parts.Length - 1) do
        if i > 0 then
            s <- s + (evaluateValue values.[i - 1])
            // s <- "\n" + (Print.printExpr 0 values[i - 1])
        s <- s + parts.[i]
    s
let transformStringTemplate ctx (com: Type.ICompiler) (generics: (string * Type) list) (parts: string list) (values: Expr list) =
    let mutable s = ""
    // Parts is the sections of the string between {}'s, so values.Length = parts.Length - 1
    // ex: printfn $"hello {1} to {2} world!" =>
        // parts = [ "hello "; " to "; " world!" ]
        // values = [ 1; 2 ]
    let rec transformValue v =
        match v with
        | Fable.TypeCast(expr, Any) -> transformValue expr
        | _ ->
            match v.Type with
            | Fable.DeclaredType(entityRef, genericArgs) ->
                com.TryGetEntity(entityRef)
                |> Option.map (fun ent -> (v, ent.FSharpFields))
                |> Error
            | _ -> Ok v
    let values = [ // Fable will show printfn values as type casts to Any, which we remove here
        for v in values do
            transformValue v
//                        match v with
//                        | TypeCast(expr, Any) -> yield expr
//                        | _ -> yield v
    ]
    for i in 0..(parts.Length - 1) do
        if i > 0 then
            match values.[i - 1] with
            | Ok v ->
//                        if values.[i - 1].Length = 1 then
                let c_type: C.Type = transformType generics v.Type
                s <- s + c_type.PrintfType
//                s <- s + (v.Type |> transformType generics).PrintfType
            | Error (Some (v, fields)) ->
                let printfComponents = System.String.Join("; ", fields |> List.map (fun f ->
                    $"{f.Name} = {(transformType generics f.FieldType).PrintfType}"))
                s <- s + $"{(transformType generics v.Type).ToTypeString()} {{ {printfComponents} }}"
            | unmatchedValue ->
                log $"Issue filling value for string template: %A{unmatchedValue}"
        s <- s + parts.[i]
    let newValues = values |> List.collect (fun v ->
        match v with
        | Ok v -> [ v ]
        | Error (Some (v, fields)) ->
            fields |> List.map (fun f ->
                let fieldInfo: Fable.FieldInfo = { Name = f.Name; FieldType = Some f.FieldType; IsMutable = f.IsMutable; MaybeCalculated = false; Tags = [] }
                Fable.Get (v, Fable.GetKind.FieldGet fieldInfo, f.FieldType, None)
            )
        | _ ->
            log $"Missing value for newValues in transform string template"
            []
    )
    let s = s.Replace("\\", "\\\\") // s.Replace("\n", "\\n")
    // C.Value (C.CStr <| s + "\\n") :: (newValues |> List.map (transformExpr ctx generics))
    C.Value (C.CStr s) :: (newValues |> List.map (transformExpr ctx generics))

let rec transformType (generics: (string * Type) list) (t: Fable.Type) =
    let rec loop (depth: int) (generics: (string * Type) list) (t: Fable.Type) =
        match t with
        | Fable.Type.Option(genericArg, isStruct) ->
            C.Type.UserDefined ($"ValueOption_{(transformType generics genericArg).ToNameString()}" , true, None)
        | Fable.Type.Tuple(genericArgs, isStruct) ->
            C.Type.UserDefined (tupleName generics genericArgs, true, None)
        | Fable.Type.Number (kind, _uom) ->
            match kind with
            | NumberKind.Int8 -> C.Char
            | NumberKind.UInt8 -> C.Byte
            | NumberKind.Int32 -> C.Int
            | NumberKind.Float32 -> C.Float
            | NumberKind.Float64 -> C.Double
            | NumberKind.UInt32 -> C.UInt32
            | NumberKind.UInt64 -> C.UInt64
            | NumberKind.Int64 -> C.Int64
            | NumberKind.NativeInt -> C.Ptr C.Void
            | NumberKind.UNativeInt -> C.EmitType "size_t"
            | _ -> C.EmitType $"/* %A{t} */"
        | Fable.Type.Boolean -> C.Bool
        | Fable.Type.Tuple (_args, _isStruct) ->
            C.EmitType $"/* %A{t} */"
        | Fable.Type.String ->
//            C.UserDefined ("System_Array__char", false, None)
            C.Ptr C.Char
//            C.UserDefined ("System_Array__char", false, None)
        | Fable.Type.Array(genericArg, arrayKind) ->
            // C.Ptr (C.UserDefined ($"System_GcArray__{(loop (depth + 1) generics genericArg).ToNameString()}", false, None))
            C.Ptr (C.UserDefined ($"System_Array__{(loop (depth + 1) generics genericArg).ToNameString()}", false, None))
    //        C.Ptr (C.EmitType $"System_Array__{(transformType generics genericArg).ToNameString()}")
    //        C.Ptr (transformType generics genericArg)
        | Fable.Type.DelegateType(argTypes, returnType) ->
            C.FunctionPtr (List.map (transformType generics) argTypes, transformType generics returnType)
        | Fable.Type.LambdaType(argType, returnType) ->
            let rec loop acc (_type: Type) =
                match _type with
                | Type.LambdaType(arg, _return) -> loop (acc @ [ arg ]) _return
                | t -> acc, t
            let uncurriedArgs, _return = loop [ argType ] returnType
            C.FunctionPtr (List.map (transformType generics) uncurriedArgs, transformType generics _return)
            //C.EmitType $"void* /* LambdaType %A{argType} %A{returnType} */"
        | Fable.Type.DeclaredType(entityRef, genericArgs) ->
            match entityRef.FullName with
            | "nativeptr`1" -> C.Ptr (transformType generics genericArgs.[0])
            | "System.NativeArray`1" -> C.Ptr (transformType generics genericArgs.[0])
            | "Microsoft.FSharp.Core.byref`2" -> C.Ptr (transformType generics genericArgs.[0])
            | s when s.StartsWith "Tmds.Linux." ->
                let s = s.Replace("Tmds.Linux.", "")
                match s with
                | "size_t" -> s
                | "ssize_t" -> s
                | "socklen_t" -> s
                | _ -> "struct " + s
                |> C.EmitType
            | _ ->
                let ent = database.contents.TryGetEntity(entityRef)
                match ent with
                | Some ent ->
                    // printfn $"Found entity: {ent.FullName}"
                    let args = genericArgs |> List.map (loop (depth + 1) generics)
                    let emitTypeAttr = ent.Attributes |> Seq.tryFind (fun attribute -> attribute.Entity.FullName = Const.emitType)
                    match emitTypeAttr with
                    | Some attribute ->
                        let typeName =
                            match attribute.ConstructorArgs.[0] with
                            | :? string as name -> name //if ent.IsValueType then "struct " + name else name
                            | _ -> ""
    //                    let t = C.UserDefined (typeName, ent.IsValueType, Some ent)
    //                    if ent.IsValueType then t
    //                    else C.Ptr t
                        if typeName = "void*" || typeName = "pthread_t" then
                            C.EmitType typeName
                        else
                            C.EmitType ("struct " + typeName)
                    | None ->
                        let t = C.UserDefined (Print.compiledTypeName (args, entityRef), ent.IsValueType, Some ent)
                        if ent.IsValueType then t
                        else C.Ptr t
                | None ->
                    printfn $"transformType: Couldn't find entity: {entityRef.FullName}\n{Environment.StackTrace}"
//                    let t = C.UserDefined (Print.compiledTypeName (args, entityRef), ent.IsValueType, Some ent)
//                    if ent.IsValueType then t
//                    else C.Ptr t
                    match entityRef.FullName with
                    | "voidptr" -> C.Ptr C.Void
                    | _ -> C.EmitType ($"/* entityRef */ {entityRef.FullName}")
        | Fable.Type.GenericParam(name, isMeasure, constraints) ->
            match generics |> List.tryFind (fun (p, _) -> p = name) |> Option.map snd with
            | Some (Fable.GenericParam(nextName, isMeasure, constraints)) when not (generics |> List.exists (fun (_name, _) -> _name = nextName && _name <> name)) ->
                C.EmitType $"/* %A{t} */"
            | Some t ->
                (loop (depth + 1) generics t)
            | _ ->
                // C.Generic name
                C.EmitType $"Type<{name}> (Generics: %A{generics})\n{Environment.StackTrace}\n"
//                C.EmitType $"void* /* %A{t} */"
//                C.Void
//                failwith "Unable to find generic param type"
        | Fable.Type.Any ->
            C.Ptr C.Void
        | Fable.Unit -> C.Void
        | Fable.Type.Char -> C.Char
        | _ -> C.EmitType $"/* %A{t} */"
    loop 0 generics t

let tupleName generics (types: Type list) =
    let typeNames =
        types |> List.map (fun t -> (transformType generics t).ToNameString().Replace("struct ", ""))
        |> String.concat "_"
    $"Tuple_{typeNames}"
let transformValueKind ctx generics (valueKind: Fable.ValueKind) =
    match valueKind with
    | Fable.NumberConstant(value, numberKind, _stringOption) ->
        match numberKind, value with
        | UInt8, (:? uint8 as value) -> C.ValueKind.Byte value
        | Int32, (:? int32 as value) -> C.ValueKind.Int value
        | UInt16, (:? uint16 as value) -> C.ValueKind.UInt16 value
        | UInt32, (:? uint as value) -> C.ValueKind.UInt32 value
        | Float32, (:? float32 as value) -> C.ValueKind.Float value
        | Float64, (:? double as value) -> C.ValueKind.Double value
        | UInt64, (:? uint64 as value) -> C.ValueKind.UInt64 value
        | Int16, (:? int16 as value) -> C.ValueKind.Int16 value
        | _ -> C.ValueKind.Emit (Print.printObj 0 value)
    | Fable.NewTuple (values, isStruct) ->
        C.ValueKind.ObjectCompound (tupleName generics (values |> List.map (fun expr -> expr.Type)), values |> List.map (transformExpr ctx generics))
    | Fable.NewOption(exprOption, ``type``, isStruct) ->
        // ValueSome
        // C.ValueKind.Emit <| "/* TODO: Fable.NewOption */" + Print.printObj 0 valueKind
        match exprOption with
        | Some expr ->
            C.ValueKind.ObjectCompound ("ValueOption_" + (transformType generics ``type``).ToNameString(), [ C.Value (C.ValueKind.Int 1); transformExpr ctx generics expr ])
        | None ->
            C.ValueKind.ObjectCompound ("ValueOption_" + (transformType generics ``type``).ToNameString(), [ C.Value (C.ValueKind.Int 0) ])

        // match exprOption with
        // | Some value ->
        //     C.ValueKind.AnonymousCompound
        // | _ ->
        //     C.ValueKind.Compound
    | Fable.StringConstant value ->
        let corrected = value.Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r")
        C.ValueKind.CStr corrected
    | Fable.CharConstant c ->
        C.ValueKind.Char c
    | Fable.UnitConstant -> // todo: this leaves blank ; around the file that makes me think soemthing is wrong D:
//        C.ValueKind.Void
        C.ValueKind.Emit $"/* %A{valueKind} */"
    | Fable.NewUnion(exprs, tag, ref, genArgs) ->
        let ent = database.contents.TryGetEntity(ref)
        match ent with
        | None -> C.ValueKind.Emit (Print.printComment valueKind)
        | Some ent ->
            let generics = genArgs |> List.zip (ent.GenericParameters |> List.map (fun p -> p.Name))
            let caseInfo = ent.UnionCases.[tag]
            let union_name = ent.FullName.Replace(".", "_")
            // todo: this can be done differently
            let inline_acquire_object (type_name: string) (expr: string) (deref: string) =
                $"({{ {type_name}* p = malloc(sizeof({type_name})); " +
                $"*p = {expr}; " +
                // todo: Runtime_track_var
                //$"Runtime_track_var((void**)&p); " +
                $"{deref}p; }})"
            let values =
                exprs
                |> List.zip caseInfo.UnionCaseFields
                |> List.map (fun (caseField, fieldExpr) ->
                    // if requiresTracking generics fieldExpr.Type then
                    //     let type_name = (transformType generics fieldExpr.Type).ToTypeString()
                    //     let value = Compiler.writeExpression (transformExpr ctx generics fieldExpr)
                    //     inline_acquire_object type_name value "*"
                    // else
                        Compiler.writeExpression (transformExpr ctx generics fieldExpr))
                |> fun values -> System.String.Join(", ", values)
            let result = $"({union_name}){{ {tag}, {{ .{caseInfo.Name} = {{ {values} }} }} }}"
            // todo: union_name for generic types
            if not ent.IsValueType then
                // [
                //     C.Statement.Expression <|
                //         C.Call ("Runtime_malloc_value", [ transformExpr ctx generics (Value (NewRecord(values, entityRef, genArgs), sourceLocationOption)) ])
                // ]
                // C.ValueKind.Emit $"({{ {union_name}* p = malloc(sizeof({union_name})); *p = {result}; Runtime_track_var((void**)&p); p; }})"
                // todo: can this be solved better so there's no deref param?
                // C.AnonymousCompound ((List.map (transformExpr ctx generics) exprs), ent, genArgs |> List.map (transformType generics))
                C.ValueKind.Emit <| inline_acquire_object union_name result ""
            else
                C.ValueKind.Emit result
    | Fable.NewRecord(values, entityRef, _genArgs) ->
//        let exprTypes = values |> List.map (fun v -> v.IsSimpleExpr)
//        let isSimpleExpr = exprTypes |> List.forall (fun simple -> simple = true)
        database.contents.TryGetEntity(entityRef)
        |> Option.map (fun entity ->
            let fieldInfo =
                entity.FSharpFields
                |> List.map (fun f -> (f.Name, transformType generics f.FieldType))
            let values = if entity.IsValueType then values else Fable.Value (Fable.ValueKind.NumberConstant (0, NumberKind.Int32, NumberInfo.Empty), None) :: values
            // let t = transformType generics entity.fu}
            let c_generics = List.map (resolveType generics) _genArgs |> List.map (transformType generics)
            let structName = Print.compiledTypeName (c_generics, entityRef)
            // let structName =
            //     match entity.Attributes |> Seq.tryFind (fun attr -> attr.Entity.FullName.Contains("EmitType")) with
            //     | Some attr -> attr.ConstructorArgs.[0]
            //     | _ ->
            //         entity.FullName.Replace(".", "_")
            //             .Replace($"`{generics.Length}", "__" + System.String.Join("_", c_generics |> List.map (fun g -> g.ToNameString())))
            //             .Replace($"${generics.Length}", "__" + System.String.Join("_", c_generics |> List.map (fun g -> g.ToNameString())))
            let value =
                C.Compound (
                    List.map (transformExpr ctx generics) values,
                    entity,
                    fieldInfo,
                    c_generics)
            if entity.IsValueType then value
            // else C.ValueKind.Emit $"({structName}*)Runtime_alloc_copy(sizeof({structName}), (void*)&{Compiler.valueToString value})"
            else C.ValueKind.Emit $"Runtime_alloc_copy(sizeof({structName}), (void*)&{Compiler.valueToString value})"
        )
        |> Option.defaultWith (fun () ->
            printfn $"============= trouble finding entity ref {entityRef.FullName} {entityRef} ================"
            (C.ValueKind.Emit <| Print.printObj 0 valueKind))
    | Fable.ThisValue ``type`` ->
        match ``type`` with
        | Fable.DeclaredType(_entityRef, _genericArgs) ->
            match database.contents.TryGetEntity(_entityRef) with
            | Some ent when ent.FullName = "Microsoft.FSharp.Core.byref`2" ->
                match _genericArgs.[0] with
                | Fable.DeclaredType(entityRef, genericArgs) ->
                    match database.contents.TryGetEntity(entityRef) with
                    | Some ent when not (ent.IsValueType) -> C.ValueKind.Emit "(*this$)"
                    | _ -> C.ValueKind.Emit "this$"
                | _ -> C.ValueKind.Emit "this$"
            | Some ent when ent.IsValueType = false ->
                C.ValueKind.Emit "(*this$)"
            | _ ->
                C.ValueKind.Emit "this$"
        | _ ->
            C.ValueKind.Emit "this$"
    | Fable.NewArray(newKind, typ, kind) ->
//        C.ValueKind.Array (values |> List.map (transformExpr ctx generics))
        let array_type_name = (transformType generics typ).ToNameString()
        match newKind with
        | Fable.ArrayValues values ->
            let unwrapCast value =
                match value with | Fable.TypeCast(expr, Any) -> expr | _ -> value
            // todo: I may need to remove this in the future when actually creating array literals
            let array_values =
                values |> List.map (unwrapCast >> transformExpr ctx generics) |> fun c_values -> String.Join(", ", c_values |> List.map (fun c_value -> Compiler.writeExpression c_value))
//            C.ValueKind.Emit <| $"{{{array_values}}}"
            let tName = (transformType generics typ).ToNameString()
            let t2 = (transformType generics typ).ToTypeString()
            let length = $"{values.Length} * sizeof({t2})"
            let arrType = $"struct System_Array__{tName}"
            // let data = $"({(transformType generics typ).ToTypeString()}[]){{ {array_values} }}"
            let data = $"{{ {array_values} }}"

            let t = (transformType generics typ)
            let tName = $"{t.ToNameString()}"
            let argsTxt = values |> List.map (transformExpr ctx generics) |> List.map Compiler.writeExpression |> String.concat ", "
            C.ValueKind.Emit $"System_Array__{tName}_ctor({values.Length}, ( ( {t.ToTypeString()}[{values.Length}] ) {{{argsTxt}}} ) )"
            // C.ValueKind.Emit $"({{ int length = {length}; {t2} data[] = {data}; void* copy = malloc({length}); memcpy(copy, data, length); {arrType}* array = malloc(sizeof({arrType})); array->length = {values.Length}; array->data = copy; array; }})"
            // C.ValueKind.Emit $"(void*)0 /* {{ %A{array_values} }} */"
        | Fable.ArrayAlloc size ->
            // todo: Won't this break with sizes that contain expressions like IfThenElse
            let size_string = transformExpr ctx generics size |> Compiler.writeExpression
            // C.ValueKind.Emit $"&(struct System_Array__{array_type_name}){{ .data = malloc(sizeof({array_type_name}) * {size_string}), .length = {size_string} }}"
            C.ValueKind.Emit $"System_Array__{array_type_name}_alloc({size_string})"
        | Fable.ArrayFrom expr ->
            C.ValueKind.Emit $"/* Fable.ArrayFrom expr {Print.printExpr 0 expr} */"
//        C.ValueKind.Ptr 1337uL
    | Fable.Null _type ->
        C.ValueKind.Emit $"/* %A{valueKind} */"
//        C.ValueKind.Ptr 1337uL
    | Fable.BoolConstant value ->
        C.ValueKind.Bool value
    | Fable.StringTemplate(exprOption, parts, values) ->
        let sprintf_args = transformStringTemplate ctx database.contents generics parts values
        let inner = Compiler.writeExpression (C.Call ("sprintf", C.Expr.Emit "buffer" :: sprintf_args))
        C.ValueKind.Emit $"({{ char buffer[2048]; {inner}; buffer;}})"
//        C.BlockExpr [
//            C.Emit "char buffer[2048];"
//            C.Expression <| C.Call ("sprintf", C.Expr.Emit "buffer" :: sprintf_args)
//            C.Emit "buffer;"
//        ] |> Compiler.writeExpression |> C.ValueKind.Emit
    | _ ->
        C.ValueKind.Emit $"/* %A{valueKind} */"
//        C.ValueKind.Ptr 1337uL
let transformOperation ctx generics (opKind: Fable.OperationKind) (_type: Type) =
    match opKind with
    | Fable.Binary(binaryOperator, left, right) ->
        let op =
            match binaryOperator with
            | BinaryPlus -> C.Add
            | BinaryMinus -> C.Minus
            | BinaryMultiply -> C.Mult
            | BinaryDivide -> C.Div
            | BinaryLess -> C.Less
            | BinaryGreater -> C.Greater
            | BinaryLessOrEqual -> C.LessOrEqual
            | BinaryModulus -> C.Modulo
            | BinaryEqual -> C.Eq
            | BinaryUnequal -> C.NotEq
            | BinaryGreaterOrEqual -> C.GreaterOrEqual
            | _ -> failwith $"Unimplemented opKind %A{opKind}"
        C.Binary (op, transformExpr ctx generics left, transformExpr ctx generics right)
    | Fable.Unary(UnaryNot, operand) ->
        C.Unary (C.UnaryOp.Not, transformExpr ctx generics operand)
    | Fable.Unary(UnaryAddressOf, operand) ->
        C.Unary (C.Ref, transformExpr ctx generics operand)
    | Fable.Unary(UnaryDeref, operand) ->
        C.Unary (C.Deref, transformExpr ctx generics operand)
//        []
//    | Logical(logicalOperator, left, right) ->
//        []
    | _ ->
        C.Expr.Emit $"(void*)0 /* {Print.printObj 0 opKind} */"

let convertCallArgs generics (callee: Fable.Expr) (callInfo: Fable.CallInfo) =
    let callArgs =
        match callInfo.MemberRef with
        | Some memberRef ->
            match database.contents.TryGetMember(memberRef) with
            | Some info ->
                let hasParamArray = info.CurriedParameterGroups.Length > 0 && info.CurriedParameterGroups.[0].Length > 0 && (List.last info.CurriedParameterGroups.[0]).Attributes |> Seq.exists (fun attribute -> attribute.Entity.FullName = "System.ParamArrayAttribute")
                ()
        //                let hasParamArray = hasParamArray info
        //                if hasParamArray then
        //                if callInfo.Args.Length > 0 then
        //                    let args =
                if callInfo.Args.Length > 0 then
                    match hasParamArray, List.last callInfo.Args with
                    | true, Value (NewArray (ArrayValues values, _, _), _) ->
                        let values = values |> List.map (fun value -> match value with | TypeCast(expr, Any) -> expr | _ -> value)
                        (callInfo.Args |> List.take (callInfo.Args.Length - 1)) @ values
                    | _ ->
                        callInfo.Args
                else []
        //                    C.Call (emitInfo.Macro, args |> List.map (transformExpr ctx generics))
        //                else C.Expr.Emit emitInfo.Macro
        //            let args =
        //                if callInfo.ThisArg.IsSome then
        //                    callInfo.ThisArg.Value :: callInfo.Args
        //                else
        //                    callInfo.Args
            | _ ->
                callInfo.Args //|> filterCallExprArgs
        | None ->
            callInfo.Args //|> filterCallExprArgs
    let transformArg (arg: Expr) (paramGroup: Parameter) =
        match (arg, paramGroup.Type) with
//        | Get(expr, FieldGet info, _type, _sourceLocationOption), DeclaredType(entityRef, genericArgs)
//                when entityRef.FullName = "Microsoft.FSharp.Core.byref`2" && info.MaybeCalculated = true && info.Name = "contents" ->
//            expr
        | _ -> arg
    if debugger.contents then
        debug "%s" (Environment.StackTrace) |> ignore
        debug "%A" callee |> ignore
        debug "%A" generics |> ignore
        debug "%A" callInfo |> ignore
        ()
        // debugger.contents <- false
    match callInfo.MemberRef |> Option.bind database.contents.TryGetMember with
    | Some _member ->
        match _member with
        | :? FsMemberFunctionOrValue as fs ->
            let callArgs, uncurriedParamGroups =
                let _params = _member.CurriedParameterGroups |> List.collect id
                callArgs, _params
//                 if (fs.Value.IsMethod) then
//                     ()
//                 if ((fs.Value.IsMemberThisValue || fs.Value.IsConstructorThisValue) && (fs.Value.IsPropertySetterMethod || fs.Value.IsPropertyGetterMethod))
//                     || (fs.Value.IsMethod) then
//                     let thisParam = { new Parameter with
//                                         member this.Attributes = Seq.empty
//                                         member this.Name = Some "this$"
//                                         member this.Type = callInfo.Args.[0].Type
//                                         member this.IsIn = false
//                                         member this.IsOut = false
//                                         member this.IsNamed = false
//                                         member this.IsOptional = false
//                                         member this.DefaultValue = None }
// //                    callInfo.Args, thisParam :: _params
//                     callArgs, thisParam :: _params
//                 elif (fs.Value.IsPropertySetterMethod || fs.Value.IsPropertyGetterMethod) && callInfo.Args.Length > 0 then
//                     //callInfo.Args.Tail, _params
//                     callArgs.Tail, _params
//                 else
// //                    callInfo.Args, _params
//                     callArgs, _params
            let callArgs =
                match callee with
                | IdentExpr ident ->
                    match ident.Type with
                    | DelegateType(argTypes, returnType) -> callee :: callArgs
                    | _ -> callArgs
                | _ -> callArgs
//            List.map2 transformArg callArgs (uncurriedParamGroups)
            callArgs
        | _ -> callArgs
    | None ->
        callArgs
// todo: Use this in transformCall
let transformCallee ctx (generics: (string * Type) list) (callInfo: CallInfo) (callee: Expr) =
    match callee with
    | IdentExpr ident ->
        match ident.Type with
        | DelegateType(argTypes, returnType) -> C.Expr.Emit ident.Name
        | _ ->
            match callInfo.MemberRef with
            | Some (MemberRef (memberRef, info)) ->
                let genericParams =
                    callInfo.GenericArgs
                    //|> List.filter (fun a -> match a with | Type.Any -> false | _ -> true)
                    |> List.map (transformType generics)
                C.Expr.Emit (Print.compiledMethodName (info.CompiledName, genericParams, memberRef))
            | _ -> C.Expr.Emit (ident.Name)
    | _ ->
        transformExpr ctx generics callee
let transformClass (generics: (string * Type) list) (com: MyCompiler) (c: ClassDecl) : C.Struct =
    let ent = com.GetEntity(c.Entity.FullName)
    let fields = ent.FSharpFields
    let struct_name = Print.compiledTypeName (generics |> List.map (snd >> transformType generics), ent.FullName)
    { tag = struct_name; members = fields |> List.map (fun f -> transformType generics f.FieldType, f.Name) }
let transformCall ctx generics (callInfo: CallInfo) (callee: Expr) (expr: Expr) =
    let isDllImport =
        callInfo.MemberRef
        |> (Option.bind database.contents.TryGetMember) |> Option.map (fun ref -> ref.Attributes)
        |> Option.defaultValue Seq.empty |> Seq.exists (fun a -> a.Entity.FullName = "System.Runtime.InteropServices.DllImportAttribute")
    let callMethod (memberRef: EntityRef) (memberInfo: MemberRefInfo) =
        let calleeCompiledName =
            let genericParams =
                callInfo.GenericArgs
                //|> List.filter (fun a -> match a with | Type.Any -> false | _ -> true)
                |> List.map (transformType generics)
            if isDllImport then
                memberInfo.CompiledName
            else Print.compiledMethodName (memberInfo.CompiledName, genericParams, memberRef)
        let callArgs = convertCallArgs generics callee callInfo |> filterCallExprArgs
        // Add this$ arg
        match callInfo.ThisArg with
        | Some this ->
            match this.Type with
            | DeclaredType(entityRef, genericArgs) ->
                match database.contents.TryGetEntity(entityRef) with
                | Some ent when ent.IsValueType ->
                    C.Call (calleeCompiledName, (C.Unary (C.UnaryOp.Ref, transformExpr ctx generics this)) :: (callArgs |> List.map (transformExpr ctx generics)))
                | _ -> C.Call (calleeCompiledName, (this :: callArgs) |> List.map (transformExpr ctx generics))
            | _ -> C.Call (calleeCompiledName, (this :: callArgs) |> List.map (transformExpr ctx generics))
        | None -> C.Call (calleeCompiledName, callArgs |> List.map (transformExpr ctx generics))
    let dependencies = Query.grabDependencies database.contents expr
    match callee with
    | Fable.IdentExpr ident ->
        match ident.Type with
        | DelegateType(argTypes, returnType) ->
            C.Call (ident.Name, callInfo.Args |> List.map (transformExpr ctx generics))
        | _ ->
            match callInfo.MemberRef with
            | Some (MemberRef (memberRef, info)) -> callMethod memberRef info // ident.Name
            | _ -> C.Call (ident.Name, callInfo.Args |> List.map (transformExpr ctx generics))
//                Print.emitComment callee
//            let calleeCompiledName = getCompiledMethodName generics callInfo database.contents memberRef ident.Name
//            // Add this$ arg
//            match callInfo.ThisArg with
//            | Some this -> C.Call (calleeCompiledName, (this :: callInfo.Args) |> List.map (transformExpr ctx generics))
//            | None -> C.Call (calleeCompiledName, callInfo.Args |> List.map (transformExpr ctx generics))
    | Import(importInfo, ``type``, sourceLocationOption) ->
        let libraryFile = IO.Path.GetFileNameWithoutExtension (Array.last (importInfo.Path.Split(char "/"))) + ".c"
        match (importInfo.Selector, libraryFile) with
        | "fromNumber", "Long.c" | "fromInteger", "Long.c" -> transformExpr ctx generics callInfo.Args[0]
        | "AllocHGlobal", "System.Runtime.InteropServices.Marshal.c" ->
            C.Call ("malloc", callInfo.Args |> List.map (transformExpr ctx generics))
        | "FreeHGlobal", "System.Runtime.InteropServices.Marshal.c" ->
            C.Call ("free", callInfo.Args |> List.map (transformExpr ctx generics))
            // C.Expr.Emit $"%A{callee}"
        | "fill", "Array.c" ->
            let t = transformType generics expr.Type.Generics[0]
            let tName = t.ToNameString()
            let sizeExpr = transformExpr ctx generics callInfo.Args[2]
            C.Call ($"System_Array__{tName}_alloc", [ sizeExpr ])
        // TODO : replace this earlier
        | "FSharpRef", "Types.c" ->
            match callInfo.Args.[0] with
            | Delegate(idents, Call(callee, callInfo, ``type``, sourceLocationOption), stringOption, tags) ->
                C.Expr.Unary (C.UnaryOp.Ref, transformExpr ctx generics callee)
            | Delegate(idents, body, stringOption, tags) ->
                C.Expr.Unary (C.UnaryOp.Ref, transformExpr ctx generics body)
            | _ ->
                C.Expr.Unary (C.UnaryOp.Ref, transformExpr ctx generics callInfo.Args.[0])
        | "toConsole", "String.c" -> Print.toConsole ctx database.contents transformType transformExpr transformValueKind generics callInfo
        | "toString", "Types.c" ->
            C.Call ((callInfo.Args[0].Type |> transformType generics).ToNameString() + "_toString", [ callInfo.Args[0] |> transformExpr ctx generics ])
        | "format", "String.c" -> Print.format ctx transformType transformExpr transformValueKind generics callInfo
        | "OfNativeIntInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            C.Expr.TypeCast (transformType generics expr.Type, (transformExpr ctx generics callInfo.Args.[0]))
        | "WritePointerInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            C.Expr.Emit $"*({Compiler.writeExpression (transformExpr ctx generics callInfo.Args.[0])}) = {Compiler.writeExpression (transformExpr ctx generics callInfo.Args.[1])}"
        | "ReadPointerInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            C.Expr.Unary (C.Deref, (transformExpr ctx generics callInfo.Args.[0]))
        | "ToNativeIntInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            C.Expr.TypeCast (C.Ptr C.Void, (transformExpr ctx generics callInfo.Args.[0]))
        | "defaultOf", "Util.c" ->
            // match expr.Type with
            // | Fable.Number _ -> C.Expr.Value (C.ValueKind.Int 0)
            // | _ ->
                // C.Expr.Emit $"(void*0) /* Unchecked.defaultOf<%A{``type``}> */" //C.Value (C.ValueKind.Void)
            let t = transformType generics expr.Type
            match t with
            | C.Float | C.Double
            | C.Int -> C.Expr.Value (C.ValueKind.Int 0)
            | _ ->
                C.Expr.Emit $"(void*)0 /* Unchecked.defaultOf<%A{t}> */" //C.Value (C.ValueKind.Void)
            // match ``type`` with
            // | Any -> C.Expr.Emit "(void*)0"
            // | _ -> C.Expr.Emit $"(void*0) /* Unchecked.defaultOf<%A{``type``}> */" //C.Value (C.ValueKind.Void)
        | "equals", "Util.c" -> C.Binary (C.BinaryOp.Eq, transformExpr ctx generics callInfo.Args.[0], transformExpr ctx generics callInfo.Args.[1])
//                C.Expr.Emit $"(void*) /* %A{expr} */"
        | "AllocHGlobal", "System.Runtime.InteropServices.c" -> C.Call ("malloc", callInfo.Args |> List.map (transformExpr ctx generics))
        | "FreeHGlobal", "System.Runtime.InteropServices.c" -> C.Call ("free", callInfo.Args |> List.map (transformExpr ctx generics))
        | "NullPointer", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            C.Expr.Emit "(void*)0 /* NullPointer */"
        | "AddPointerInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            C.Expr.Binary (C.Add, transformExpr ctx generics callInfo.Args.[0], transformExpr ctx generics callInfo.Args.[1])
        | "GetPointerInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            C.IndexedAccess (transformExpr ctx generics callInfo.Args.[0],transformExpr ctx generics callInfo.Args.[1])
        | "SetPointerInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
//            transformExpr ctx generics callInfo.Args.[0]
            C.Expr.ExprAssignment (C.IndexedAccess (transformExpr ctx generics callInfo.Args.[0],transformExpr ctx generics callInfo.Args.[1]), transformExpr ctx generics callInfo.Args.[2])
//            C.Expr.ExprAssignment (C.Unary (C.Deref, (C.Expr.Binary (C.Add, transformExpr ctx generics callInfo.Args.[0], transformExpr ctx generics callInfo.Args.[1]))), transformExpr ctx generics callInfo.Args.[2])
        | "ToByRefInlined", "Microsoft.FSharp.NativeInterop.NativePtrModule.c" ->
            transformExpr ctx generics callInfo.Args.[0]
        | "sizeof", "operators.c" -> C.Expr.Emit <| "sizeof(" + (transformType generics callInfo.GenericArgs.[0]).ToTypeString() + ")"
        | selector, file when (List.contains "new" callInfo.Tags) ->
            // C.Expr.Emit (Print.printObj 0 callee + "\n" + Print.printObj 2 callInfo)
            C.Expr.Emit "{}"
        | _, _ ->
            match importInfo.Kind with
            | ClassImport entityRef ->
                Print.emitComment callee
            | MemberImport (mem) ->
                match mem with
                | MemberRef.MemberRef (memberRef, info) ->
                    match database.contents.TryGetMember(mem) with
                    | Some ent ->
                        let attributes = ent.Attributes
                        match (attributes |> Seq.tryFind (fun attr -> attr.Entity.FullName = "System.Runtime.InteropServices.DllImportAttribute")) with
                        | Some attr ->
                            C.Call (info.CompiledName, callInfo.Args |> List.map (transformExpr ctx generics))
                        | _ ->
                            callMethod memberRef info
                    | None ->
                        // log $"Missing info for %A{memberRef}\n%A{info}\n"
                        // Print.emitComment importInfo
                        C.Call (memberRef.FullName + "_" + info.CompiledName, callInfo.Args |> List.map (transformExpr ctx generics))
                        // |> fun expr -> log $"%A{expr}"; expr
                | _ ->
                    // Print.emitComment(importInfo)
                    Print.emitComment "yo"
//                let compiledName = getCompiledMethodName generics callInfo database.contents memberRef importInfo.Selector
//                C.Call (compiledName, callInfo.Args |> List.map (transformExpr))
            | _ ->

                C.Expr.Emit <| Print.printComment ("Unmatched import", libraryFile, importInfo, expr)
                //Print.emitComment(importInfo)
    | Get(IdentExpr ident, FieldGet field, ``type``, sourceLocationOption) when ident.Name = "console" && field.Name = "log" ->
        let args = callInfo.Args |> List.map (transformExpr ctx generics)
        let args =
            // Append the necessary \n (happens with Console.WriteLine)
            match args[0] with
            | C.Value (C.ValueKind.CStr value) -> (C.Value (C.ValueKind.CStr <| value + "\\n")) :: (List.tail args)
            | _ -> args
        C.Call ("printf", args)
    | Get(expr, kind, ``type``, sourceLocationOption) ->
//        Print.emitComment callee
        let isJsKeyword =
            match expr with
            | IdentExpr ident -> Naming.jsKeywords.Contains ident.Name
            | _ -> false
        match callInfo.MemberRef with
        | Some (MemberRef(declaringEntity, memberRefInfo)) ->
            let ent = database.contents.TryGetEntity(declaringEntity)
            C.Expr.Emit (Print.printComment expr + "\n" + (ent |> Option.map (fun e -> Print.printComment e) |> Option.defaultValue ""))
        | _ ->
            Print.emitComment expr
        //Print.emitComment (expr, kind)
//        C.Expr.Emit <| $"// %A{expr} %A{kind}".Replace("\n", "")
    | _ ->
        C.Expr.Emit $"(void*)0 /* {Print.printComment expr} */"

let inline tryAs (f: 'a -> 't) (t: 't) : 'a option =
    if FSharpType.IsUnion(typeof<'t>) then
        let caseInfo = FSharpType.GetUnionCases(typeof<'t>)
        let defaultValue =
            if FSharpType.IsTuple typeof<'a> then
                let elements = FSharpType.GetTupleElements typeof<'a>
                FSharpValue.MakeTuple ([| for _ in 1..elements.Length do yield null |], typeof<'a>) :?> 'a
            else
                Unchecked.defaultof<'a>
        let case = f defaultValue
        let (valueCaseInfo, values) = FSharpValue.GetUnionFields(t, typeof<'t>)
        let (expectedCaseInfo, _) = FSharpValue.GetUnionFields(case, typeof<'t>)
        if valueCaseInfo.Tag = expectedCaseInfo.Tag then
            if FSharpType.IsTuple typeof<'a> then
                Some (FSharpValue.MakeTuple(values, typeof<'a>) :?> 'a)
            else
                Some (values.[0] :?> 'a)
        else
            None
    else
        None

let transformExpr (ctx: Context) (generics: (string * Type) list) (expr: Expr) : C.Expr =
    try
        match expr with
    //    | ObjectExpr(memberDecls, ``type``, exprOption) ->
    //        C.Unknown
        | Fable.Unresolved (e, typ, range) ->
            match e with
            | Fable.UnresolvedExpr.UnresolvedReplaceCall (thisArg, args, info, attached) ->
                match info.DeclaringEntityFullName, info.CompiledName with
                | "Microsoft.FSharp.Core.Operators", "SizeOf" ->
                    C.Expr.Call ("sizeof", [ C.Expr.Emit <| (transformType generics info.GenericArgs.[0]).ToTypeString() ])
                | _ ->
                    Print.emitComment expr
            | _ ->
                Print.emitComment expr
        | Import(importInfo, ``type``, sourceLocationOption) ->
            match importInfo.Selector with
            | "toConsole" -> C.Expr.Emit "printf"
            | _ -> Print.emitComment expr // C.Expr.Emit importInfo.Selector
        | IdentExpr ident ->
            match isArgValueThis generics database.contents ident with
            | Some ident -> C.Ident (ident, true)
            | _ -> C.Ident (ident, isValueType ident.Type)
        | Operation(operationKind, tags, ``type``, _sourceLocationOption) ->
            transformOperation ctx generics operationKind ``type``
        | Call(callee, callInfo, _type, _sourceLocationOption) ->
            transformCall ctx generics callInfo callee expr
        | Value(StringTemplate(exprOption, parts, values), sourceLocation) ->
            let sprintf_args = transformStringTemplate ctx database.contents generics parts values
            let sprintf_args = transformStringTemplate ctx database.contents generics parts values
            let inner = Compiler.writeExpression (C.Call ("sprintf", C.Expr.Emit "buffer" :: sprintf_args))
            C.Expr.Emit $"({{ char buffer[2048]; {inner}; buffer;}})"
    //        C.BlockExpr [
    //            C.Emit "char buffer[2048];"
    //            C.Expression <| C.Call ("sprintf", C.Expr.Emit "buffer" :: sprintf_args)
    //            C.Emit "buffer;"
    //        ]
        | Value(valueKind, _sourceLocationOption) ->
            C.Value <| transformValueKind ctx generics valueKind
        | Test(expr1, testKind, sourceLocationOption) ->
            let comment = Print.emitComment expr
            match testKind with
            | ListTest isCons -> comment
            | OptionTest isSome ->
                let item = C.MemberAccess (transformExpr ctx generics expr1, "tag")
                C.Binary (C.BinaryOp.Eq, item, C.Value (C.ValueKind.Int (if isSome then 1 else 0)))
            | TypeTest ``type`` -> comment
            | UnionCaseTest tag ->
                let access =
                    match expr1.Type with
                    | DeclaredType (entRef, genArgs) ->
                        let ent = compiler.GetEntity(entRef.FullName)
                        if ent.IsValueType then C.MemberAccess
                        else C.DerefMemberAccess
                    | _ ->
                        C.MemberAccess
                C.Binary (C.BinaryOp.Eq, access (transformExpr ctx generics expr1, "union_tag"), C.Value (C.ValueKind.Int tag))
        | Get(expr, kind, _type, _sourceLocationOption) ->
            let isThisArguments =
                match expr with
                | IdentExpr ident ->
                    ident.IsThisArgument || ident.Name = "this$"
                | _ -> false
            let isThis =
                match expr with
                | Fable.Value (Fable.ThisValue typ, _range) ->
                    true
                | _ -> false
            let exprIsByref =
                match expr.Type with
                | DeclaredType(entityRef, genericArgs) -> entityRef.FullName = Const.byrefType || entityRef.FullName = Const.byrefType2
                | _ -> false
            match kind with
    //        | FieldGet info when info.MaybeCalculated = true && info.Name = "contents" ->
    //            C.Expr.Unary (C.Deref, (transformExpr ctx generics expr))
            | FieldGet info ->
                // For reference type values we need to deref the object before accessing the field
                let accessType =
                    match expr.Type with
                    | DeclaredType(entityRef, genericArgs) ->
                        match entityRef.FullName with
                        | "Microsoft.FSharp.Core.byref`2" | "Microsoft.FSharp.Core.ByRefKinds.InOut" ->
                            C.DerefMemberAccess
                        | _ ->
                            match database.contents.TryGetEntity(entityRef) with
                            | Some ent ->
                                if isThis then C.MemberAccess
                                elif not ent.IsValueType &&
                    //                                           // Extension methods for structs have a this arg that's passed as a byref
                                      (not (isThisArguments && exprIsByref)) then C.DerefMemberAccess
                                   elif exprIsByref && isThisArguments then C.DerefMemberAccess
                                   elif ent.FullName.Contains "Array" then C.DerefMemberAccess
                                   else C.MemberAccess
                            | None -> C.DerefMemberAccess
                    | Array(genericArg, arrayKind) -> C.DerefMemberAccess
                    | _ -> C.MemberAccess
                match expr.Type with
                | String ->
                    C.Expr.Emit <|
                        "strlen(" + (Compiler.writeExpression (transformExpr ctx generics expr)) + ")"
                // | Array (typ, kind: ArrayKind) ->
                //     C.DerefMemberAccess(transformExpr ctx generics expr, info.Name)
                | _ -> accessType (transformExpr ctx generics expr, info.Name)
    //            match expr.Type with
    //            | DeclaredType(entityRef, genericArgs) ->
    //                database.contents.TryGetEntity(entityRef)
    //                |> Option.bind (fun ent ->
    //                    if ent.IsValueType then Some <| C.DerefMemberAccess (transformExpr ctx generics expr, info.Name)
    //                    else None
    //                )
    //            | _ ->
    //                None
    //            |> Option.defaultValue
    //                (C.MemberAccess (transformExpr ctx generics expr, info.Name))
            | ExprGet getExpr ->
                match expr.Type with
                | Array(genericArg, arrayKind) ->
                    C.Call($"System_Array__{(transformType generics genericArg).ToNameString()}_get_Item", [
                        transformExpr ctx generics expr
                        transformExpr ctx generics getExpr
                    ])
                | _ ->
                    C.IndexedAccess (transformExpr ctx generics expr, transformExpr ctx generics getExpr)
            | OptionValue ->
                C.MemberAccess (transformExpr ctx generics expr, "value")
            | TupleIndex index ->
                C.MemberAccess (transformExpr ctx generics expr, $"value_{index}")
            | UnionField fieldInfo ->
                let ent = database.contents.GetEntity(fieldInfo.Entity)
                let caseName = ent.UnionCases.[fieldInfo.CaseIndex].Name
                let fieldName = ent.UnionCases.[fieldInfo.CaseIndex].UnionCaseFields.[fieldInfo.FieldIndex].Name
                let access =
                    match expr.Type with
                    | DeclaredType (entRef, genArgs) ->
                        let ent = compiler.GetEntity(entRef.FullName)
                        if ent.IsValueType then C.MemberAccess
                        else C.DerefMemberAccess
                    | _ ->
                        C.MemberAccess
                C.MemberAccess (C.MemberAccess (access (transformExpr ctx generics expr, "union_data"), caseName), fieldName)
            | a ->
                C.MemberAccess (transformExpr ctx generics expr, $"%A{a}")
        | Emit(emitInfo, _type, _sourceLocationOption) ->
    //        transformEmit generics emitInfo
            let emitInfo = { emitInfo with CallInfo = { emitInfo.CallInfo with Args = convertCallArgs generics expr emitInfo.CallInfo } }
            match emitInfo.Macro with
            | "IsValueType" ->
                match tryResolveType generics emitInfo.CallInfo.GenericArgs.[0] with
                | Some t ->
                    match t with
                    | DeclaredType(entityRef, genericArgs) ->
                        let e = database.contents.GetEntity(entityRef)
                        C.Expr.Value <| C.ValueKind.Bool (e.IsValueType)
                    | Array(genericArg, arrayKind) ->
                        C.Expr.Value <| C.ValueKind.Bool false
                    | _ -> // todo: some of these aren't value types
                        C.Expr.Value <| C.ValueKind.Bool true
                | None ->
                    failwith $"Couldn't resolve type info for IsValueType, emitInfo =  %A{emitInfo}"
            | "____finalize_function_pointer" ->
                match tryResolveType generics emitInfo.CallInfo.GenericArgs.[0] with
                | Some (DeclaredType(entityRef, genericArgs)) ->
    //                C.Emit $"&{}"
                    let c_types = List.map (transformType generics) genericArgs
                    C.Expr.Emit $"&{Print.finalizerName (c_types, entityRef)}"
    //                C.Call (Print.finalizerName (List.map (transformType generics) genericArgs, entityRef), [ transformExpr ctx generics emitInfo.CallInfo.Args.[0] ])
                | _ -> C.Expr.Emit $"/* %A{expr} */"
            | "____var_args" ->
                match emitInfo.CallInfo.Args.[0] with
                | IdentExpr ident ->
                    match compiler.GetIdents() |> List.tryFind (fun (i, _) -> i.Name = ident.Name) with
                    | Some (ident, value) ->
                        match value with
                        | Value(NewArray(ArrayValues values, ``type``, arrayKind), sourceLocationOption) ->
                            let values = values |> List.map (fun value -> match value with | TypeCast(expr, Any) -> expr | _ -> value)
                            C.Expr.Emit (values |> List.map (fun value -> (transformExpr ctx generics value) |> Compiler.writeExpression) |> fun call_args -> String.Join(", ", call_args))
                        | _ ->
                            C.Expr.Emit ident.Name
                    | _ ->
                        C.Expr.Emit ""
                | _ ->
                    C.Expr.Emit ""
            | "____em_asm_method_var_args"
            | "____em_asm_var_args" ->
                match emitInfo.CallInfo.Args.[0] with
                | IdentExpr ident ->
                    match compiler.GetIdents() |> List.tryFind (fun (i, _) -> i.Name = ident.Name) with
                    | Some (ident, value) ->
                        match value with
                        | Value(NewArray(ArrayValues values, ``type``, arrayKind), sourceLocationOption) ->
                            let isMethod = if emitInfo.Macro = "____em_asm_method_var_args" then true else false
                            let values = if isMethod then List.skip 1 values else values
                            C.Expr.Emit (values |> List.mapi (fun i _ -> $"${if isMethod then i + 1 else i}") |> fun em_asm_args -> String.Join(", ", em_asm_args))
                        | _ ->
                            C.Expr.Emit ""
                    | _ ->
                        C.Expr.Emit ""
                | _ ->
                    C.Expr.Emit ""
            | "____include_package" ->
                match emitInfo.CallInfo.Args.[0] with
                | Value(StringConstant header, sourceLocationOption) ->
                    includedPackages.Value <- header :: includedPackages.Value
                    C.Expr.Emit ""
                | _ ->
                    C.Expr.Emit ""
            | "____conditional_include" ->
                match emitInfo.CallInfo.Args.[0] with
                | Value(StringConstant packageName, sourceLocationOption) ->
                    // todo: conditional_packages
                    // conditional_packages.Value <- conditional_packages.Value.Add(compiler.CurrentFile, packageName)
                    C.Expr.Emit ""
                | _ ->
                    C.Expr.Emit ""
            | "____emit_header_text" ->
                match emitInfo.CallInfo.Args.[0] with
                | Value(StringConstant text, sourceLocationOption) ->
                    header_emit_texts.Value <- text :: header_emit_texts.Value
                    C.Expr.Emit ""
                | _ ->
                    C.Expr.Emit ""
            | "____include_header" ->
                match emitInfo.CallInfo.Args.[0] with
                | Value(StringConstant header, sourceLocationOption) ->
                    includedHeaders.Value <- header :: includedHeaders.Value
                    C.Expr.Emit ""
                | _ ->
                    C.Expr.Emit ""
            | "____literal" ->
                transformExpr ctx generics (emitInfo.CallInfo.Args.[0])
            | "____typeLiteral" ->
    //            C.Call (emitInfo.CallInfo.GenericArgs)
                match emitInfo.CallInfo.GenericArgs.[0] with
                | DeclaredType(entityRef, genericArgs) ->
                    let typeName = Print.compiledTypeName ((List.map (transformType generics) genericArgs), entityRef)
                    C.Expr.Emit typeName
                | _ ->
                    C.Expr.Emit ((transformType generics emitInfo.CallInfo.GenericArgs.[0]).ToNameString())
            | "____finalize" ->
    //            C.Call (emitInfo.CallInfo.GenericArgs)
                match emitInfo.CallInfo.GenericArgs.[0] with
                | DeclaredType(entityRef, genericArgs) ->
                    C.Call (Print.finalizerName (List.map (transformType generics) genericArgs, entityRef), [ transformExpr ctx generics emitInfo.CallInfo.Args.[0] ])
                | _ -> C.Expr.Emit $"/* %A{expr} */"
            | "____create_handle" ->
                C.Call ("Runtime_create_handle", [ transformExpr ctx generics emitInfo.CallInfo.Args.[0]; transformExpr ctx generics emitInfo.CallInfo.Args.[1] ])
            | "____release_handle" ->
                C.Call ("Runtime_release_handle", [ transformExpr ctx generics emitInfo.CallInfo.Args.[0] ])
            | "____to_nativeptr" ->
    //            match emitInfo.CallInfo.GenericArgs.[0] with
    //            | DeclaredType(entityRef, genericArgs) when not (database.contents.GetEntity(entityRef).IsValueType) ->
    //                transformExpr ctx generics
    //            | _ ->
                transformExpr ctx generics emitInfo.CallInfo.Args.[0]
            | "____deref_as_nativeptr" ->
                C.Expr.Unary (C.Ref, transformExpr ctx generics emitInfo.CallInfo.Args.[0])
            | "cast" ->
                printfn $"{Print.printExpr 0 expr}"
                let target = (transformType generics emitInfo.CallInfo.GenericArgs.[1])
                printfn $"Target = {Print.printObj 0 target}"
                let src = transformExpr ctx generics emitInfo.CallInfo.Args.[0]
                printfn $"Src = {Print.printObj 0 src}"
                C.Expr.TypeCast (target, src)
            | "emit" ->
                match emitInfo.CallInfo.Args.[0] with
                | Value(StringConstant value, sourceLocationOption) ->
                    C.Expr.Emit value
                | Value(StringTemplate(exprOption, parts, values), _) ->
                    let compiler = database.contents
                    C.Expr.Emit (emitEvaluateStringTemplate ctx compiler generics parts values)
                | _ ->
                    Print.emitComment expr
            | _ ->
                match emitInfo.CallInfo.MemberRef with
                | Some (MemberRef(declaringEntity, memberRefInfo)) ->
    //                let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
    //                    let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
    //                        if memb.CurriedParameterGroups.Count <> 1 then false else
    //                        let args = memb.CurriedParameterGroups[0]
    //                        args.Count > 0 && args[args.Count - 1].IsParamArrayArg
    //
    //                    let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
    //                        Seq.tryLast memb.CurriedParameterGroups
    //                        |> Option.bind Seq.tryLast
    //                        |> Option.map (fun lastParam -> hasAttribute Atts.paramList lastParam.Attributes)
    //                        |> Option.defaultValue false
    //                    hasParamArray memb || hasParamSeq memb
                    match database.contents.TryGetMember(emitInfo.CallInfo.MemberRef.Value) with
                    | Some info ->
                        let hasParamArray = info.CurriedParameterGroups.Length > 0 && info.CurriedParameterGroups.[0].Length > 0 && (List.last info.CurriedParameterGroups.[0]).Attributes |> Seq.exists (fun attribute -> attribute.Entity.FullName = "System.ParamArrayAttribute")
                        let emitInfo = { emitInfo with CallInfo = { emitInfo.CallInfo with Args = (convertCallArgs generics expr emitInfo.CallInfo) } }
        //                let hasParamArray = hasParamArray info
        //                if hasParamArray then
                        if emitInfo.CallInfo.Args.Length > 0 then
                            let args =
                                match hasParamArray, List.last emitInfo.CallInfo.Args with
                                | true, Value (NewArray (ArrayValues values, _, _), _) ->
                                    let values = values |> List.map (fun value -> match value with | TypeCast(expr, Any) -> expr | _ -> value)
                                    (emitInfo.CallInfo.Args |> List.take (emitInfo.CallInfo.Args.Length - 1)) @ values
                                | _ ->
                                    emitInfo.CallInfo.Args
                            C.Call (emitInfo.Macro, args |> filterCallExprArgs |> List.map (transformExpr ctx generics))
                        else
                            Print.emitComment emitInfo
                    | _ ->
                        // Print.emitComment emitInfo
                        C.Call (emitInfo.Macro, emitInfo.CallInfo.Args |> List.map (transformExpr ctx generics))
        //                else
        //                    C.Call (emitInfo.Macro, emitInfo.CallInfo.Args |> List.map (transformExpr ctx generics))
                | _ ->
                    C.Call (emitInfo.Macro, emitInfo.CallInfo.Args |> List.map (transformExpr ctx generics))
    //        let mutable toEmit = emitInfo.Macro
    //        for i in 0..(emitInfo.CallInfo.Args.Length - 1) do
    //            toEmit <- toEmit.Replace($"${i}", emitInfo.CallInfo.Tags)
    //        C.Expr.Emit (emitInfo)

    //    | DecisionTreeSuccess(targetIndex, boundValues, typ) ->
            // todo: what is typ for?
            // todo: boundValues ?
    //        C.Value (C.ValueKind.Int targetIndex)
    //        C.Expr.ExprAssignment (C.Expr.Emit "decisionTree", C.Value (C.ValueKind.Int targetIndex))

        | TypeCast(expr, ``type``) ->
    //        match expr.Type, ``type`` with
    //        | Number(NumberKind.Int32, numberInfo), Number(NumberKind.UInt8) -> C.Expr.Emit $"(() & 0xFF)"
            C.TypeCast (transformType generics ``type``, transformExpr ctx generics expr)
            // C.Expr.Emit (Print.printExpr 0 expr + "\n" + Print.printObj 0 ``type``)
        | Let(_ident, _value, body) ->
            // todo: this needs to be able to return additional pre-pend statements
            // todo: or, we need to make sure that the ident has been declared already
    //        C.Ident ident
            transformExpr ctx generics body
            //C.BlockExpr (transformMember ctx generics expr)
        | Lambda(ident, body, _stringOption) ->
    //        transformLambda generics ident body
            // printfn $"Transforming lambda: {Print.printExpr 0 expr}"
            let uncurriedArgs, uncurriedBody = unwrapLambda ident body
            printfn $"====================Lambda: {Print.printExpr 0 expr}"
            // printfn $"Uncurried: %A{uncurriedArgs}"
            // printfn $"Uncurried: %A{uncurriedBody}"
    //        C.FunctionPtr (List.map (transformType generics) uncurriedArgs, transformType generics _return)
            if Query.isEmptyDelegate uncurriedArgs uncurriedBody then
                printfn "Is empty lambda"
                let (Call (callee, callInfo, _, _)) = uncurriedBody
                //transformExpr ctx generics
                transformCallee ctx generics callInfo callee
            else
                printfn "Is not-empty lambda"
                // wip: check for an anonymous_fn implementation
                C.Expr.Emit $"{getAnonymousFunctionName ctx.currentFile expr}"
    //            C.Expr.Emit $"(void*)0 /* {Print.printComment expr} */"
        | CurriedApply(applied, exprs, ``type``, sourceLocationOption) ->
            let lambdaDepth (t: Fable.Type) =
                let rec loop n t =
                    match t with
                    | Fable.LambdaType (argType, returnType) ->
                        match returnType with
                        | Fable.LambdaType _ -> loop (n + 1) returnType
                        | _ -> n + 1
                    | _ ->
                        n
                loop 0 t
            match applied with
            | IdentExpr ident ->
                C.Call (ident.Name, exprs |> List.map (transformExpr ctx generics))
            | Fable.Call(callee, info, Fable.LambdaType (argType, returnType), range) ->
                if lambdaDepth applied.Type = exprs.Length then
                    Print.emitComment expr
                else
                    Print.emitComment expr
            | _ ->
                Print.emitComment expr
        | Delegate(idents, body, stringOption, tags) ->
            transformDelegate ctx generics expr idents body
        | IfThenElse(guardExpr, thenExpr, elseExpr, _sourceLocationOption) ->
            C.Ternary (transformExpr ctx generics guardExpr, transformExpr ctx generics thenExpr, transformExpr ctx generics elseExpr)
        | ObjectExpr (members, typ, baseCall) ->
            // log $"%A{members}"
            // log $"%A{typ}"
            // log $"%A{baseCall}"
            if members.Length = 0 then
                C.Expr.Value C.ValueKind.Void
            else
                Print.emitComment expr
        | _ ->
            Print.emitComment expr
    with e ->
        Print.emitComment (string e)

let getAnonymousFunctionName (filename: string) (expr: Expr) =
    let idents =
        match expr with
        | Delegate (idents, body, stringOption, tags) -> idents
        | Lambda(ident, body, stringOption) -> unwrapLambda ident body |> fst
        | _ -> [] // shouldn't happen
    let file = filename.Split(char "/") |> Array.last
    let file = file.Replace(".", "_")
    let range =
        match expr.Range with
        | Some range -> range
        | None ->
            let hasRange (e: Expr) = if e.Range.IsSome then Some [ e ] else None
            let identRanges = idents |> List.map (fun i -> i.Range)
            let (nextExprWithRange, range) =
                expr
                |> walkExpr (fun e -> if e.Range.IsSome then Some (e, e.Range.Value) else None)
                |> List.find Option.isSome |> Option.get
                // expr
                // |> Fable.Transforms.AST.visitFromOutsideIn (fun e -> if e.Range.IsSome && e.Range.Value <> Unchecked.defaultof<_>
                //                                                         then Some e
                //                                                         else None)
            range // todo
            // match (nextExprWithRange.Range :: identRanges) |> List.tryPick id with
            // | Some r -> r
            // | _ ->
            //     Unchecked.defaultof<_>
//                |> Option.defaultValue (fun () -> List.map (Fable.Transforms.AST.visitFromOutsideIn hasRange) idents)
//            nextExprWithRange.Range.Value
    $"{file}_anonymous_fn_{range.start.line}"
let transformDelegate (ctx: Context) (generics: (string * Type) list) (expr: Expr) (idents: Ident list) (body: Expr) : C.Expr =
    match body with
    | Call(callee, callInfo, ``type``, sourceLocationOption) ->
        match callInfo.MemberRef with
        | Some (MemberRef(declaringEntity, memberRefInfo)) ->
//                let function_name = declaringEntity.FullName.Replace(".", "_")
            // If all of the args are equal to the delegate idents, we don't have to create an anonymous_fn
                //transformExpr ctx generics callee
            if Query.isEmptyDelegate idents body then
                transformCallee ctx generics callInfo callee
            else
                match expr.Range with
                | Some range ->
                    C.Expr.Emit (getAnonymousFunctionName ctx.currentFile expr)
//                    C.Expr.Emit $"/* emit_debug */ {function_name}_{memberRefInfo.CompiledName}"
                | _ -> C.Expr.Emit <| $"/* couldn't find range for delegate */" + Print.printComment expr
//                transformExpr ctx generics callee
        | _ ->
            if Query.isEmptyDelegate idents body then
                (C.Unary (C.Ref, transformExpr ctx generics callee))
            else
                match expr.Range with
                | Some range -> C.Expr.Emit (getAnonymousFunctionName ctx.currentFile expr)
                | None -> Print.emitComment expr
    | CurriedApply(applied, exprs, ``type``, sourceLocationOption) ->
        if Query.isEmptyDelegate idents body then
            transformExpr ctx generics applied
        else
            Print.emitComment expr
    | _ ->
        for i in 1..10 do
            printfn "Checking lambda in transformDelegate (third case)"
        C.Expr.Emit (getAnonymousFunctionName ctx.currentFile expr)

let rec getExprType generics (e: Expr) : C.Type =
    match e with
    | Let(ident, value, body) -> getExprType generics body
    | Value(valueKind, sourceLocationOption) -> transformType generics valueKind.Type
    | IdentExpr ident -> transformType generics ident.Type
    | Sequential exprs -> transformType generics (List.last exprs).Type
    | _ -> C.Void

let transformNewRecord ctx generics expr =
    match expr with
    | Value(NewRecord (values, entityRef, genArgs), sourceLocationOption) ->
        if values |> List.forall Query.isSimpleExpr then
            [ C.Statement.Expression <| transformExpr ctx generics expr ]
        else
//            let setupStatements = values |> List.map (fun v -> [
//                if Query.isComplexExpr v then
//    //                        yield! [ C.Statement.Expression <| transformExpr context v ]
//    //                    else
//                    let items: C.Statement list = transformMember ctx generics v
//                    yield! items |> Seq.take (items.Length - 1)
//            ]
                // todo: return an ident expr
//            )
            let values = [
                for v in values do
                    if Query.isComplexExpr v then
        //                        yield! [ C.Statement.Expression <| transformExpr context v ]
        //                    else
                        let items: C.Statement list = transformMember ctx generics v
                        let id = Guid.NewGuid().ToString()
                        // Since c declarations can't start with a number, find the first char that isn't a number or _
                        let name = id.Replace("-", "_").Substring(id |> Seq.tryFindIndex (fun c -> "0123456789_".Contains(string c) = false) |> Option.defaultValue 0)
                        let _type = getExprType generics v
                        let value = C.DeclarationAssignment.StatementAssignment << C.Block <| items
                        let ident = C.Ident ({
                            Name = name; Type = v.Type; IsMutable = false; IsThisArgument = false; IsCompilerGenerated = false; Range = None;
                        }, (isValueType v.Type))
                        yield (ident, [ C.Declaration { _type = _type; name = name; value = value; requiresTracking = failwith "todo" } ])
                    else
                        yield (transformExpr ctx generics v, [])
                ]
            let toReturn =
                values |> List.collect snd
//                setupStatements |> List.reduce (@)
            let entity = database.contents.GetEntity(entityRef)
            let fieldInfo =
                entity.FSharpFields
                |> List.map (fun f -> (f.Name, transformType generics f.FieldType))
            // ((List.map (transformExpr context) values), entity, fieldInfo)
            toReturn @ [ C.Statement.Expression << C.Value << C.Compound <|
//                         ((values |> List.map (transformExpr ctx generics)),
                          ((values |> List.map fst),
                          entity,
                          fieldInfo, []) ]
    | _ -> []
let identsToWatch =
    [
        //"mythread"
        //"l2"
    ]
let rec requiresTracking generics t =
    match t with
    | DeclaredType(entityRef, genericArgs) ->
        match database.contents.TryGetEntity(entityRef) with
        | Some e when e.IsValueType -> false
        | Some e ->
            log $"Requires Tracking: {e.FullName}"
            true
        | _ ->
            match entityRef.FullName with
            | "nativeptr`1" -> false
            | _ -> true
    | Array _ -> true
    | List _ -> true
    | Any -> true
    | Tuple(genericArgs, isStruct) -> not isStruct || (List.exists (requiresTracking generics) genericArgs)
    | GenericParam(name, isMeasure, constraints) -> requiresTracking generics (resolveType generics t)
    | AnonymousRecordType(fieldNames, genericArgs, isStruct) -> not isStruct || (List.exists (requiresTracking generics) genericArgs)
    | Boolean -> false
    | String -> false
    | Char -> false
    | _ ->
        false
let transformLet ctx generics (ident: Ident) value (body: Expr) =
    if Seq.contains ident.Name identsToWatch then ()
    let ctx = { ctx with idents = ident.Name :: ctx.idents }
    // printfn $"{Print.printObj 0 ident}"
    // printfn $"{Print.printExpr 0 value}"
    let c_value =
        // For let expr's that can be translated into a single statement in C
        if Query.exprIsDefaultOf value then
            C.Default
        else
            match Query.isSimpleExpr value with
            | true ->
                C.ExprAssignment (transformExpr ctx generics value)
            | false ->
                match value with
                // todo: do we need this? or nah?
    //            | Delegate(idents, expr, stringOption, tags) ->
    //                let name = $"{ident.Name}__anonymous_fn__{ident.Range.Value.start.line}"
    //                C.ExprAssignment (C.Expr.Emit name)
                    //let f = transformFunc name idents body generics
                    // todo: generics gathering
    //                    context.Value <-
    //                        { context.Value with compiled_module = context.Value.compiled_module |> Map.add name f }
    //            | Let(ident, Call(Import(importInfo, _, _), callInfo, ``type``, sourceLocationOption), body)
    //                    when ident.Name.StartsWith("inputRecord") && importInfo.Selector = "FSharpRef" && importInfo.Path.EndsWith("/Types.js") ->
    //                if Query.isSimpleExpr body then
    //                    C.ExprAssignment (transformExpr ctx generics (replaceInputRecordIdent body ident.Name callInfo.Args.[0]))
    //                else
    //                    C.StatementAssignment << C.Block <| (transformMember ctx generics (replaceInputRecordIdent body ident.Name callInfo.Args.[0]))
                | Delegate(idents, body, stringOption, tags) ->
                    // todo: emit anonymous_fn
                    C.ExprAssignment (Print.emitComment (Let (ident, value, body)))
                | Lambda(ident, body, stringOption) ->
                    let args, body = unwrapLambda ident body
                    // todo; is closure
                    // let isClosure = Fable.Transforms.Rust.Fable2Rust.Util.hasCapturedIdents
                    // todo: fails for lambdas that use recursion
                    // List.tail ctx.idents is used since the most recent binding will be the lambda itself
                    if not (Query.isClosure (List.tail ctx.idents) body) then
                    // if Query.isEmptyDelegate args body then
                        let name = getAnonymousFunctionName ctx.currentFile value
                        C.ExprAssignment <| C.Expr.Emit name
                    else
                        // todo
                        let name = getAnonymousFunctionName ctx.currentFile value
                        C.ExprAssignment <| C.Expr.Emit name
                | _ ->
                    // todo declare everything before
                    // For more complex assignment, we have to declare the type and then initialize it in a code block
                    C.StatementAssignment << C.Block <| (transformMember ctx generics value)
    let variable_type =
        printfn $"Variable type: %A{ident.Type}"
        match transformType generics ident.Type with
        | C.Void -> C.Ptr C.Void
        | t -> t
    let variable_type =
        match value, variable_type with
        // | Value(NewRecord _, _), C.EmitType name -> C.EmitType ("struct " + name)
        | _ -> variable_type
    let assignment = C.Declaration { C.DeclarationInfo._type = variable_type
                                     C.DeclarationInfo.name = ident.Name
                                     C.DeclarationInfo.value = c_value
                                     C.DeclarationInfo.requiresTracking = requiresTracking generics value.Type }
    compiler.AddIdent (ident, value)
    let following =
        let bodyType = body.Type
        let body = transformMember ctx generics body
        let postFollowing =
            if requiresTracking generics value.Type then
                // log $"================ TODO: Create macro for Runtime_end_var_scope, simpler than handling it in this section ===================="
                // todo: end_var_scope
                let name =
                    match variable_type with
                    | C.Ptr t | t -> t.ToNameString()
                let finalizer_id = $"{name}_Destructor"
                // [ C.Expression <| C.Call ("Runtime_end_var_scope", [ C.TypeCast (C.EmitType "void**", C.Expr.Emit $"&{ident.Name}"); C.Expr.Emit ("(void*)" + finalizer_id) ]) ]
                []
                // [ C.Emit $"{ident.Name}->__refcount--;" ]
            else
                []
        if body.Length > 0 then
            if bodyType = Type.Unit then
                body @ postFollowing
            else
                body @ postFollowing
                // todo: This doesn't work with returnsCounter
                // (List.take (body.Length - 1) body) @ postFollowing @ [ List.last body ]
        else
            postFollowing @ [ List.last body ]
    compiler.RemoveIdent ident |> ignore
    let postAssignment =
        if requiresTracking generics value.Type && Query.isSimpleExpr value then
            // todo: Runtime_track_var
            // [ C.Expression <| C.Call ("Runtime_track_var", [ C.TypeCast (C.EmitType "void**", C.Expr.Emit $"&{ident.Name}")]) ]
            // [ C.Expression <| C.Call ("Runtime_inc_count", [ C.TypeCast (C.EmitType "void*", C.Expr.Ident (ident, false)) ]) ]
            [ C.Emit $"{ident.Name}->__refcount++;" ]
        else
            []
    let assignment =
        let sb = SourceBuilder()
        (assignment :: postAssignment) // |> Compiler.writeStatements sb
        // C.Emit <| sb.ToString().Replace(";\n", "; ")
    assignment @ following // @ postFollowing
    // Print.emitComment value

let isValueType (t: Type) =
    match t with
    | DeclaredType(entityRef, genericArgs) ->
        (database.contents.TryGetEntity(entityRef) |> Option.map (fun e -> e.IsValueType) |> Option.defaultValue true)
    | Array(genericArg, arrayKind) -> false
    | _ -> true
let wrapStatementsWithThreadContextUpdate (statements: C.Statement list) =
    [
        // C.Emit "// __thread_context++;"
        yield! statements
        // C.Emit "// __thread_context--;"
    ]
let debugIdent (ident: Ident) =
    $"{ident.Name}"

let debugExpr (depth: int) (expr: Expr) =
    if displayExpressions then
        let result =
            match expr with
            | Let(ident, value, body) -> $"Expr.Let ({debugIdent ident}, {debugExpr depth value}"
            | _ -> $"%A{fst <| Reflection.FSharpValue.GetUnionFields (expr, expr.GetType())}"
        System.String.Join("\t", [0..depth] |> List.map (fun _ -> "")) + " " + result
    else ""
let applyTransformations (expr: Expr) : Expr =
    // todo: is this working ?
    let expr = expr |> walkExprInPlace replacements
    expr
let grabLastExpr (callback: Expr -> Expr option) expr =
    let mutable lastExpr = None
//        let createGetUnionField n = boundValues.[n]
//        let rec loop n (bindings: Ident list) expr =
//            match bindings with
//            | [] -> expr
//            | [ binding ] -> Let (binding, createGetUnionField n, Value (ValueKind.UnitConstant, binding.Range)) // expr)
//            | binding :: remainingBindings -> Let (binding, createGetUnionField n, loop (n + 1) remainingBindings expr)
    expr |> Fable.Transforms.AST.visitFromOutsideIn (fun e ->
        match callback e with
        | Some e ->
            if lastExpr = None then lastExpr <- Some e
            Some e
        | None -> None
    // todo should this actually be ignored?
    ) |> ignore
    lastExpr
let initial_attempt_transformDecisionTreeConditionExpr generics (expr: Expr) (targets: (Ident list * Expr) list) =
    let range =
        expr.Range |> Option.map (fun r -> $"{r.start.line}_{r.start.column}") |> Option.defaultValue "_empty_range_expr"
    let decisionTreeResultName = $"decisionTree_{range}"
//    let transformDecisionTreeSuccess (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
    let transformLeaveContext (t: Type option) (e: Expr) =
        e
    let getDecisionTargetAndBindValues (targetIndex: int) (boundValues: Expr list) : (Ident * Expr) list * Expr =
        [], Value (ValueKind.UnitConstant, None)
//    let transformDecisionTreeSuccess targetIndex boundValues =
//        let bindings, target = getDecisionTargetAndBindValues targetIndex boundValues
//        match bindings with
//        | [] ->
//            transformLeaveContext None target
//        | bindings ->
//            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
//            transformLeaveContext None target
//    let callback (e: Expr) : Expr option =
//        match e with
//        | DecisionTreeSuccess(targetIndex, boundValues, ``type``) ->
////            transformLeaveContext targetIndex boundValues
//            let typ = Type.Number (NumberKind.Int32, NumberInfo.Empty)
//            let ident = {
//                Name = decisionTreeResultName
//                Type = typ; IsMutable = false; IsThisArgument = false
//                IsCompilerGenerated = true // todo should this be true?
//                Range = expr.Range
//            }
//            let target = targets.[targetIndex]
//            let createGetUnionField n =
//                // todo: which index?
//                let value = boundValues.[n]
//                let ent =
//                    match value.Type with
//                    | DeclaredType(entityRef, genericArgs) -> entityRef
//                    | _ -> Unchecked.defaultof<_>
//                // todo: CaseIndex vs FieldIndex ?
//                let typ = Type.Any
//                value
//                //Get (value, GetKind.UnionField { Entity = ent; GenericArgs = []; CaseIndex = n; FieldIndex = n; }, typ, None)
//            let rec loop n (bindings: Ident list) expr =
//                match bindings with
//                | [] -> expr
//                | [ binding ] -> Let (binding, createGetUnionField n, Value (ValueKind.UnitConstant, binding.Range)) // expr)
//                | binding :: remainingBindings -> Let (binding, createGetUnionField n, loop (n + 1) remainingBindings expr)
////            loop 0 (fst targets.[targetIndex])
//            Some <| (loop 0 (fst targets.[targetIndex]) (snd targets.[targetIndex]))
////            Some <| Sequential [ (loop 0 (fst targets.[targetIndex]) (snd targets.[targetIndex])); (snd targets.[targetIndex]) ]
////            let createLetBindings
////            Some ((snd targets.[targetIndex]))
//            //Some (Fable.Set (IdentExpr ident, SetKind.ValueSet, typ, Value <| (ValueKind.NumberConstant (targetIndex, NumberKind.Int32, NumberInfo.Empty), e.Range), e.Range))
//        | _ -> None
    // todo declare everything before
    // For more complex assignment, we have to declare the type and then initialize it in a code block
//    let value =
//        C.StatementAssignment << C.Block <| (transformMember ctx generics (Fable.Transforms.AST.visitFromOutsideIn callback expr))
//    let assignment = C.Declaration { C.DeclarationInfo._type = C.Int
//                                     C.DeclarationInfo.name = $"decisionTree_{range}"
//                                     C.DeclarationInfo.value = value }

//    let declarations = (grabLastExpr callback expr)
//    let e = Fable.Transforms.AST.visitFromInsideOut id expr
//    let compiledValues = transformMember ctx generics declarations

    // todo: this used to be named declarations
//    let expr = (Fable.Transforms.AST.visitFromOutsideIn callback expr)
//    [ yield! transformMember ctx generics declarations; (transformMember )]
//    (transformMember ctx generics (Fable.Transforms.AST.visitFromOutsideIn callback expr))

//    [ yield! compiledValues; yield! transformMember ctx generics expr ]

    // let following = transformMember ctx generics body
    // assignment :: following
//    [
////        C.Declaration { _type = C.Int; name = $"decisionTree_{range}"; value = C.StatementAssignment << C.Block <| (transformMember ctx generics expr) }
//        assignment
////        yield! [ C.Statement.Expression <| Print.emitComment expr ] @
//        yield! (targets |> List.map (snd >> transformMember ctx generics) |> List.collect id)
//    ]
    []
let transformDecisionTreeConditionExpr ctx generics (expr: Expr) (targets: (Ident list * Expr) list) =
    let applyTarget (e: Expr) : Expr option =
        match e with
        | DecisionTreeSuccess(targetIndex, boundValues, ``type``) ->
            // todo: original attempt for reference
//            let rec loop n (bindings: Ident list) expr =
//                match bindings with
//                | [] -> expr
//                | [ binding ] -> expr //Let (binding, createGetUnionField n, Value (ValueKind.UnitConstant, binding.Range)) // expr)
//                | binding :: remainingBindings -> loop (n + 1) remainingBindings expr //Let (binding, createGetUnionField n, loop (n + 1) remainingBindings expr)
//            loop 0 (fst targets.[targetIndex])

            let rec loop n (bindings: (Ident * Expr) list) expr =
                match bindings with
                | [] -> expr
                | [ (ident, value) ] ->
                    Let (ident, value, expr)
//                    Let (binding, createGetUnionField n, Value (ValueKind.UnitConstant, binding.Range)) // expr)
                | (ident, value) :: remainingBindings ->
                    Let (ident, value, loop (n + 1) remainingBindings expr)
//                    Let (binding, boundValues)
//                    loop (n + 1) remainingBindings expr //Let (binding, createGetUnionField n, loop (n + 1) remainingBindings expr)
            let (idents, matchSuccessExpr) = targets.[targetIndex]
            let bindings = List.zip idents boundValues
            Some (loop 0 bindings matchSuccessExpr)
//            Some (loop 0 (fst targets.[targetIndex]))

            //Some <| (loop 0 (fst targets.[targetIndex]) (snd targets.[targetIndex]))
//            Some (snd targets.[targetIndex])
        | _ -> None
    let expr1 = (Fable.Transforms.AST.visitFromOutsideIn applyTarget expr)
//    let expr2 = (grabLastExpr applyTarget expr)
//    let result = transformMember ctx generics expr
    let result = transformMember ctx generics expr1
    result
let inline third (a: 'a * 'b * 'c) =
    let (_, _, c) = a
    c
let debugger = ref false
let transformMember ctx (generics: (string * Type) list) (body: Expr) =
    let body = body |> applyTransformations
    //let body = body |> walkExprInPlace replaceByrefContents
    //let body = body |> walkExprInPlace replaceEmitTypeCallArgs
//    let body = body |> walkExprInPlace (replaceByrefArguments database.contents)
    let rec loop generics body =
        // Making sure that nothing inside of the loop calls the outer function
        let transformMember = null
        if displayExpressions then
            printfn $"{debugExpr 0 body}"
        match body with
    //    | Operation(operationKind, ``type``, sourceLocationOption) ->
    //        [ C.Statement.Expression <| transformOperation operationKind ``type`` ]
        | Sequential exprs -> exprs  |> List.collect (loop generics)
        | Let(ident, value, letBody) ->
            // if ctx.currentFile.Contains "audio.fs" then printfn $"{Print.printExpr 0 body}"
            transformLet ctx generics ident value letBody
        | IdentExpr ident -> [ C.Statement.Expression <| C.Ident (ident, isValueType ident.Type) ]
        | DecisionTree(expr1, targets) ->
            transformDecisionTreeConditionExpr ctx generics expr1 targets
        | WhileLoop(guard, expr, _sourceLocationOption) ->
            wrapStatementsWithThreadContextUpdate [ C.WhileLoop (transformExpr ctx generics guard, loop generics expr) ]
        | ForLoop(ident, start, limit, expr, isUp, _sourceLocationOption) ->
            let condition =
                C.Binary ((if isUp then C.LessOrEqual else C.GreaterOrEqual), C.Ident (ident, isValueType ident.Type), transformExpr ctx generics limit)
            let each =
                C.Assignment (C.Ident (ident, isValueType ident.Type), C.Binary (C.Add, C.Ident (ident, isValueType ident.Type), C.Value (C.ValueKind.Int (if isUp then 1 else -1))))
            let declaration: C.DeclarationInfo =
                {
                    _type = transformType generics ident.Type
                    name = ident.Name
                    value = C.ExprAssignment (transformExpr ctx generics start)
                    requiresTracking = false
                }
            wrapStatementsWithThreadContextUpdate [ C.ForLoop (declaration, condition, each, loop generics expr) ]
        | Set(expr, kind, _type, value, _sourceLocationOption) ->
            // log $"========================= set ============================="
            // log $"{Print.printObj 0 expr.Type}"
            // log $"%A{expr.Type}"
            let expr =
                match expr.Type with
                // | DeclaredType(entityRef, genericArgs)
                //         when entityRef.FullName = Const.byrefType || entityRef.FullName = Const.byrefType2 ->
                //     Operation (OperationKind.Unary (UnaryOperator.UnaryDeref, expr), [], genericArgs.[0], expr.Range)
                | _ -> expr
            let value =
                match expr.Type with
                | DeclaredType(entityRef, genericArgs) when entityRef.FullName = Const.byrefType2 && genericArgs.[0] = value.Type ->
                    Operation (OperationKind.Unary (UnaryOperator.UnaryAddressOf, value), [], expr.Type, value.Range)
                | DeclaredType(entityRef, genericArgs) when entityRef.FullName = Const.byrefType && genericArgs.[0] = value.Type ->
                    Operation (OperationKind.Unary (UnaryOperator.UnaryAddressOf, value), [], expr.Type, value.Range)
                | _ -> value
            let result =
                match kind with
                | FieldSet fieldName ->
                    let accessType =
                        match expr with
                        // | DeclaredType (entityRef, a) ->
                        //     match entityRef.FullName with
                        //     | "Microsoft.FSharp.Core.byref`2" ->
                        //         C.DerefMemberAccess
                        //     | _ ->
                        //         database.contents.TryGetEntity entityRef
                        //         |> Option.map (fun e -> if e.IsValueType then C.MemberAccess else C.DerefMemberAccess)
                        //         |> Option.defaultValue C.MemberAccess
                        //     // C.DerefMemberAccess
                        | IdentExpr ident when isByRefType expr.Type ->
                            C.DerefMemberAccess
                        | IdentExpr ident when ident.Name = "this$" && isByRefType expr.Type ->
                            C.DerefMemberAccess
                        | _ ->
                            C.MemberAccess
                    if requiresTracking generics value.Type then
                        let e = transformExpr ctx generics value
                        let _member = (accessType ((transformExpr ctx generics expr), fieldName))
                        let (C.Ptr valueType) = transformType generics value.Type
                        // todo: reassign
                        // [ C.Emit $"Runtime_swap_value((void**)&{Compiler.writeExpression _member}, {Compiler.writeExpression e});" ]
                        [
                            C.Emit $"Runtime_swap_value((void**)&{Compiler.writeExpression _member}, {Compiler.writeExpression e}, {valueType.ToNameString()}_Destructor);"
                            // C.Assignment ((accessType ((transformExpr ctx generics expr), fieldName)), transformExpr ctx generics value)
                        ]
                    else
                        [
                            // C.Expression <| Print.emitComment body
                            // C.Emit (sprintf "%A" body)
                            C.Assignment ((accessType ((transformExpr ctx generics expr), fieldName)), transformExpr ctx generics value)
                        ]
                // todo: replaced by replaceByrefContents
    //            | ExprSet (Value (StringConstant string, _)) when string = "contents" ->
    //                 [
    //                     if not <| Query.isSimpleExpr value then
    //                         C.Declaration {
    //                             name = $"temp_value_{_sourceLocationOption.Value.start.line}"
    //                             _type = transformType generics value.Type
    //                             value = C.StatementAssignment (C.Block (loop generics value))
    //                         }
    //                     C.Assignment (C.Unary <| C.UnaryExpr (C.Deref, transformExpr ctx generics expr), if Query.isSimpleExpr value then transformExpr ctx generics value else C.Expr.Emit $"temp_value_{_sourceLocationOption.Value.start.line}")
    //                     //yield! beforeAssignment @
    //                ]
        //            [ C.Statement.Expression (C.Expr.ExprAssignment (C.Unary <| C.UnaryExpr (C.Deref, transformExpr ctx generics expr), transformExpr ctx generics value)) ]
        //            [ C.Assignment (transformExpr ctx generics expr, transformExpr ctx generics value) ]
                | ExprSet setExpr ->
                    match expr.Type with
                    | Array (_genericArg, _kind) -> //[ C.Assignment ((C.IndexedAccess (transformExpr ctx generics expr, transformExpr ctx generics setExpr)),
                                                                                    //transformExpr ctx generics value) ]
                        [ C.Statement.Expression (C.Call ($"System_Array__{(transformType generics _genericArg).ToNameString()}_set_Item", [
                            transformExpr ctx generics expr
                            transformExpr ctx generics setExpr
                            transformExpr ctx generics value
                        ])) ]
                    | _ ->
                        [ C.Emit $"/* %A{body} */" ]
                | ValueSet ->
                    let e = transformExpr ctx generics expr
                    let _value = transformExpr ctx generics value
                    [
                        if requiresTracking generics value.Type then
                            C.Emit $"Runtime_reassign_var((void**)&{Compiler.writeExpression e}, {Compiler.writeExpression _value});"
                        else
                            C.Assignment (e, _value);
                    ]
            result
        | IfThenElse(guardExpr, thenExpr, elseExpr, _sourceLocationOption) ->
            if Query.isSimpleExpr guardExpr then
                let guard = transformExpr ctx generics guardExpr
                match guard with
                | C.Value (C.ValueKind.Bool true) ->
                    loop generics thenExpr
                | C.Value (C.ValueKind.Bool false) ->
                    loop generics elseExpr
                | _ ->
                    [ C.Conditional (transformExpr ctx generics guardExpr,
                                    loop generics thenExpr,
                                    loop generics elseExpr) ]
            else
                if Flags.UseBlockExprForConditionalGuards then
                    [ C.Conditional (C.BlockExpr <| loop generics guardExpr,
                                     loop generics thenExpr,
                                     loop generics elseExpr) ]
                else
                // BlockExpr works actually, but the below hack helps out the DecisionTree process
                // todo: I think what i need to do is check if the last expression of a DecisionTree(expr1, _)
                // todo: is an IfThenElse, and then do this hack there. IfThenElse shouldn't be weird for the DecisionTree's sake
                    let range = body.Range |> Option.map (fun r -> $"{r.start.line}_{r.start.column}") |> Option.defaultValue "something_random_todo_fixme"
                    [
                        C.Declaration {
                            name = $"result_{range}"
                            _type = C.Bool
                            value = C.StatementAssignment << C.Block <|
                                    loop generics guardExpr
                            // todo: requiresTracking for IfThenElse
                            requiresTracking = false
                        }
                        C.Conditional (C.Expr.Emit $"result_{range}", loop generics thenExpr, loop generics elseExpr)
                    ]
//                let setup_statements = guard |> List.take (guard.Length - 1)
//                let result =
//                    match List.last guard with
//                    | C.Expression expr ->
//                        expr
//                    | _ ->
//                        Print.emitComment guardExpr
//                [
//                    yield! setup_statements
////                    C.Emit ("bool result = " + Compiler.writeExpression result)
//                    C.Declaration { name = "result"; _type = C.Bool; value = C.StatementAssignment << C.Block <| guard }
//                    C.Conditional (C.Expr.Emit "result", loop generics thenExpr, loop generics elseExpr)
//                    C.Conditional (result, loop generics thenExpr, loop generics elseExpr)
//                ]
        | TryCatch(expr, _tupleOption, _exprOption, _sourceLocationOption) -> loop generics expr
        | Value(valueKind, _sourceLocationOption) ->
            match valueKind with
            | NewRecord _ -> transformNewRecord ctx generics body
            | _ -> [ C.Statement.Expression <| transformExpr ctx generics body ]
        | _ -> [ C.Statement.Expression <| transformExpr ctx generics body ]
    loop generics body
    //        [ C.Statement.Expression (C.Value (C.ValueKind.Int 0)) ]

//let count = ref 0
//let inline transformMemberDebug generics body =
//    if true then
//        //C.Emit $"/* %A{body} */" :: transformMember ctx generics body
//        transformMember ctx generics body
//    else
//        count := count.Value + 1
//        let display = Display.fableExpression count.Value body |> Seq.take (count.Value + 1) |> Seq.toArray
//        for i in 0..(display.Length - 2) do
//            printf $"{display.[i]}"
//        let duCase = (Seq.last display).Replace("\n", "")
//        let translation = transformMember ctx generics body
//        printf "%A" (List.head translation)
//        for _ in 1..2 do
//            printf "    "
//        for i in 0..(display.Length - 2) do
//            printf $"{display.[i]}"
//        printfn $"    {duCase}"
//        count := count.Value - 1
//        translation

let transformMemberDeclArgs generics (args: Ident list) : Ident list * C.Statement list =
    let args_and_statements =
        args |> List.map (fun arg ->
            match arg.Type with
            | Fable.DeclaredType(entityRef, genericArgs) when entityRef.FullName = "Microsoft.FSharp.Core.byref`2" && arg.IsThisArgument ->
                match (tryResolveType generics genericArgs.[0]) with
                | Some (DeclaredType(entityRef, _)) ->
                    match database.contents.TryGetEntity(entityRef) with
                    | Some ent when ent.IsValueType ->
                        arg, []
                        // { arg with Name = "this_value"; Type = resolveType generics genericArgs.[0] }, [ C.Declaration {
                        //     _type = C.Ptr (transformType generics genericArgs.[0])
                        //     name = "this$"
                        //     value = C.ExprAssignment (C.Unary (C.Ref, C.Expr.Emit "this_value"))
                        // } ]
                    | _ -> arg, []
                | _ -> arg, []
            | _ -> arg, []
        )
    let args = args_and_statements |> List.map fst |> filterFunctionMethodArgs
    let statements = args_and_statements |> List.collect snd
    (args, statements)
//    match database.contents.TryGetMember(memberDecl.MemberRef) with
//    | Some m when m.DeclaringEntity.IsSome ->
//        match database.contents.TryGetEntity(m.DeclaringEntity.Value) with
//        | Some ent ->
//        | _ -> memberDecl.Args
//    | _ -> memberDecl.Args
