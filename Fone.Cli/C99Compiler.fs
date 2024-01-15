module Fable.C.C99Compiler

open System
#if !FABLE_COMPILER
open System.Net.Sockets
#endif
open System.Threading
open Fable
open Fable.AST
open Fable.AST.Fable

// type Compiler = ICompiler
type Context = {
    currentFile: string
    idents: string list
}

type FileCompilationResults = {
    classDeclarations: Map<string, ClassDecl>
    memberDeclarations: Map<string * string, MemberDecl * AST.C.FunctionInfo>
    extraIncludes: string Set
    memberRefs: Map<string, MemberFunctionOrValue>
} with
    member this.AddClassDeclaration(c: ClassDecl) = { this with classDeclarations = this.classDeclarations.Add (c.Entity.FullName, c) }
    member this.AddMemberDeclaration(m: MemberDecl * AST.C.FunctionInfo) =
        match (fst m).MemberRef with
        | MemberRef (ent, info) -> { this with memberDeclarations = this.memberDeclarations.Add ((ent.FullName, (fst m).Name), m) }
        | _ -> this
    member this.AddInclude(s: string) = { this with extraIncludes = this.extraIncludes.Add s }
module FileCompilationResults =
    let AddClassDeclaration (c: ClassDecl) (this: FileCompilationResults) = this.AddClassDeclaration c
    let AddMemberDeclaration ((m, ent, functionInfo): MemberDecl * MemberFunctionOrValue * AST.C.FunctionInfo) (this: FileCompilationResults) =
        let this = this.AddMemberDeclaration (m, functionInfo)
        let m = ent //database.contents.GetMember(m.MemberRef)
        { this with memberRefs = Map.add m.FullName m this.memberRefs }
    let AddInclude (s: string) (this: FileCompilationResults) = this.AddInclude s
    let Empty _ _ = { memberRefs = Map.empty; classDeclarations = Map.empty; memberDeclarations = Map.empty; extraIncludes = Set.empty }

// http://www.fssnip.net/1V/title/Clojures-Atoms
type Atom<'T when 'T : not struct>(value : 'T) =
    let mutable atomContents = value
    
    let rec swap f =
        let currentValue = atomContents
        let resultingValue = f currentValue
        #if FABLE_COMPILER
        atomContents <- resultingValue
        #else
        let result = Interlocked.CompareExchange<'T>(&atomContents, resultingValue, currentValue)
        if obj.ReferenceEquals(result, currentValue) then resultingValue
        else Thread.SpinWait 20; swap f
        #endif
        
    member self.Value with get() = atomContents
    member self.Swap (f : 'T -> 'T) = swap f

        

type MyCompiler() =
    let mutable results: Map<string, FileCompilationResults> = Map.empty
    let mutable files = []
    let mutable entities: Map<string, Entity> = Map.empty
    let _members: Atom<Map<string, MemberFunctionOrValue>> = Atom(Map.empty)
    let idents = Atom([])
    let mutable genericClassDefs = Atom(Map.empty)
    member this.UpdateFile(file: string, update:  'a -> FileCompilationResults -> FileCompilationResults, args: 'a) =
        if not (results.ContainsKey file) then
            results <- results.Add (file, { memberRefs = Map.empty; classDeclarations = Map.empty; memberDeclarations = Map.empty; extraIncludes = Set.empty })
            files <- file :: files
        try
            results <- results.Add (file, update args results.[file])
        with ex ->
            printfn $"{ex}"
    member this.AddEntity(ent: Entity) =
        entities <- entities.Add (ent.FullName.Split(char "`").[0], ent)
    member this.GetEntity(fullName: string) = entities.[fullName.Split(char "`").[0]]
    member this.TryGetEntity(fullName: string) = entities.TryFind(fullName.Split(char "`").[0])
    member this.GetMember(fullName: string) : MemberFunctionOrValue =
        _members.Value.[fullName]
    member this.Members = _members
    member this.AddMember(m: MemberFunctionOrValue) : unit =
        _members.Swap (Map.add m.FullName m) |> ignore
    member this.BroadcastFinishedCompilation() =
        #if !FABLE_COMPILER
        use udpClient = new UdpClient()
        let msgBytes = Text.Encoding.ASCII.GetBytes("Compilation.Finished")
        try
            let numBytesSent = udpClient.Send(msgBytes, msgBytes.Length, "127.0.0.1", 8424)
            ()
        with ex ->
            printfn $"%A{ex}"
        #else
        ()
        #endif
//    member this.UpdateFile(file: string, update: 'a -> FileCompilationResults -> FileCompilationResults, args: 'a) =
//        this.UpdateFile(file, update, args)
    member this.Files = List.rev files // results.Keys
    member this.SetFiles _files = files <- _files
    member this.GetResult(file: string) = results.[file]
    member this.genericClassDeclarations = genericClassDefs
    member this.AddIdent(ident: Ident, value: Expr) = // todo: this needs to be per file worker D:, maybe move this into a variable in transformFile
        idents.Swap (List.insertAt 0 (ident, value)) |> ignore
    member this.RemoveIdent(ident: Ident) =
        idents.Swap (fun idents -> idents |> List.filter (fun (i, _) -> i.Name <> ident.Name))
    member this.GetIdents() =
        idents.Value
    member this.AddGenericClassDecl (c: ClassDecl) =
        genericClassDefs.Swap (Map.add c.Entity.FullName c)
        |> ignore
