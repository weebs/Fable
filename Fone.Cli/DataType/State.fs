module Fone.DataType.State

open Fable.C.AST
open Fable.AST.Fable
open Fable.Transforms.FSharp2Fable
type Filename = string
type VariableName = string


let genericMethodDeclarations: Map<string * string, MemberDecl> ref = ref Map.empty
let initDeclarations: Map<string, ActionDecl list * C.FunctionInfo> ref = ref Map.empty
let fileOrder: string list ref = ref []
let extraIncludes: Set<string> ref = ref Set.empty
let moduleStaticVars: Map<Filename, Map<VariableName, MemberDecl>> ref = ref Map.empty

let addModuleStaticVar (file: string) ((name, value): string * MemberDecl) =
    if not (moduleStaticVars.Value.ContainsKey file) then
        // moduleStaticVars += (file, Map.empty.Add(name, value))
        moduleStaticVars.contents <- moduleStaticVars.contents.Add(file, Map.empty.Add(name, value))
    else
        // moduleStaticVars += (file, moduleStaticVars.Value.[file].Add(name, value))
        moduleStaticVars.contents <- moduleStaticVars.contents.Add(file, moduleStaticVars.contents.[file].Add(name, value))
        
