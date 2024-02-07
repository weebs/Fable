module clang_ast_parser.Test

open Ast
open FParsec.CharParsers
open NUnit.Framework

[<Test>]
let ``function pointer with function pointer argument`` () =
    let text =
        "int64_t (*)(GDExtensionObjectPtr, void (*)(void *, uint32_t), void *, int, int, GDExtensionBool, GDExtensionConstStringPtr)"
    // match run Ast.Parser.Type.cType text with
    // | Success(foo, unit, position) ->
    //     Assert.Pass()
    // | Failure(s, parserError, unit) ->
    //     Assert.Pass()
    match run Ast.Parser.Type.functionPointer text with
    | Success(foo, unit, position) ->
        Assert.Pass()
    | Failure(s, parserError, unit) ->
        Assert.Fail()
    match run Ast.Parser.Type.parse text with
    | Success(argType, unit, position) ->
        Assert.Pass()
    | Failure(s, parserError, unit) ->
        Assert.Fail()
[<Test>]
let ``gdextension GDExtensionInterfaceWorkerThreadPoolAddNativeGroupTask test`` () =
    let text =
        "|-TypedefDecl 0x1dbad6ed770 <line:1786:1, col:261> col:19 GDExtensionInterfaceWorkerThreadPoolAddNativeGroupTask 'int64_t (*)(GDExtensionObjectPtr, void (*)(void *, uint32_t), void *, int, int, GDExtensionBool, GDExtensionConstStringPtr)'"
    match run Ast.Parser.typedefDecl text with
    | Success ((((result, name), typ), emptyMaybe), state, pos) ->
        match run Parser.Type.parse typ with
        | Success(argType, unit, position) ->
            Assert.Pass()
        | Failure(s, parserError, unit) ->
            Assert.Pass()
    | Failure(s, parserError, unit) ->
        Assert.Pass()
