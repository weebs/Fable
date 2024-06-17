module Fone.Database

open Fable
open Fable.AST.Fable
open Fable.C
open Fable.C.C99Compiler

module Parsing =
    open Fable.AST.Fable
    type console =
        static member log (o: obj) = printfn $"{o}"
    type Log =
        | Info of string
        | Debug of string
        | CompilationUnits of info: string
        | StartSection of title: string
        | MissingEntity of EntityRef * MemberRefInfo
    type Message =
        // | AddFile of Fable.Compiler * string * File
        // | SetCompiler of Fable.Compiler
        | AddEntity of File * Entity
        | AddMember of MemberRef * File * Entity * MemberFunctionOrValue * MemberDecl
        | AddMemberWithoutEntity of File * EntityRef * MemberRefInfo * MemberDecl
        // | FsiStarted of fsi: FsiEvaluationSession * (FsiEvaluationSessionHostConfig * TextWriter * StringBuilder)
        // | AddAsyncCallback of callback: (Fable.Compiler * Fable.AST.Fable.File -> Async<unit>)
        // | AddCallback of callback: (Fable.Compiler * Fable.AST.Fable.File -> unit)
    let logLevel = 1
    let logAgent = MailboxProcessor<Log>.Start(fun inbox -> async {
        let mutable missingEntities = Set.empty
        let mutable memberRefs = Set.empty
        while true do
            let! msg = inbox.Receive()
            match msg with
            | Info s when logLevel = 0 -> console.log $"{s}"
            | MissingEntity (entityRef, memberRefInfo) ->
                if not <| missingEntities.Contains entityRef then
                    // console.log $"Missing Entity\n%A{entityRef}"
                    missingEntities <- missingEntities.Add entityRef
                // if not <| memberRefs.Contains memberRefInfo then
                    // console.log $"Missing Entity for Member\n%A{memberRefInfo}"
                    // memberRefs <- memberRefs.Add memberRefInfo
            | Debug text ->
                console.log $"{text}"
            | CompilationUnits text ->
                console.log $"{text}"
            | StartSection title ->
                console.log $"====================================================================="
                console.log $"{title}"
                console.log $"====================================================================="
            | _ -> () // console.log $"%A{msg}"
    })
    let parseDecl (log: Log -> unit) (com: Fable.Compiler) fileName file (decl: Declaration) = [
        match decl with
        | MemberDeclaration memberDecl ->
            // if Helpers.Query.isGenericDecl memberDecl && memberDecl.Name.Contains "AddItem" then
            //     print_debug "%s" (Helpers.Print.printExpr 0 memberDecl.Body)
            log <| Info $"Method: {memberDecl.Name}"
            match memberDecl.MemberRef with
            | MemberRef(declaringEntity, memberRefInfo) ->
                match com.TryGetEntity(declaringEntity) with
                | Some ent ->
                    yield AddEntity (file, ent)
                    match ent.TryFindMember(memberRefInfo) with
                    | Some m ->
                        yield AddMember (memberDecl.MemberRef, file, ent, m, memberDecl)
                    | _ ->
//                        console.log $"Missing member:\n%A{memberRefInfo}"
                        log <| Debug $"Missing member: {memberRefInfo.CompiledName} in {fileName}"
                | None ->
//                    console.log $"Missing entity {declaringEntity.FullName} for member {memberRefInfo.CompiledName}"
                    log <| MissingEntity (declaringEntity, memberRefInfo)
                    yield AddMemberWithoutEntity (file, declaringEntity, memberRefInfo, memberDecl)
            | GeneratedMemberRef generatedMember ->
                log <| Info $"%A{generatedMember}"
        | ActionDeclaration actionDecl ->
            log <| Info $"ActionDecl {fileName}"
        | ModuleDeclaration moduleDecl ->
            log <| Info $"%A{moduleDecl}"
        | ClassDeclaration classDecl ->
            let isGeneric = classDecl.Name.Contains "$"
            // todo: Can we put this somewhere more noticeable/not mixed in with other stuff?
            if not isGeneric then
                Transforms.compiler.UpdateFile(fileName, FileCompilationResults.AddClassDeclaration, classDecl)
            match com.TryGetEntity classDecl.Entity with
            | Some ent ->
                yield AddEntity (file, ent)
                let ref = classDecl.Constructor |> Option.map (fun m -> m.MemberRef)
                match ref, Option.bind com.TryGetMember ref with
                | Some ref, Some m -> yield AddMember (ref, file, ent, m, classDecl.Constructor.Value)
                | _ -> ()
            | None ->
                log <| Debug $"Couldn't find entity for {classDecl.Entity.FullName}"
                log <| Info $"Class: %A{classDecl.Entity.FullName}"
                let ref = classDecl.Constructor |> Option.map (fun m -> m.MemberRef)
                match ref, Option.bind com.TryGetMember ref with
                | Some (MemberRef (ent, info)), Some m -> yield AddMemberWithoutEntity (file, ent, info, classDecl.Constructor.Value)
                | _ -> ()
    ]
    let fableDeclAgent (dispatch: Message -> unit) (com: Fable.Compiler) = fun (inbox: MailboxProcessor<string * File * Declaration>) -> async {
        while true do
            let! (fileName, file, decl) = inbox.Receive()
            let items = parseDecl logAgent.Post com fileName file decl
            items |> List.iter dispatch
    }

module CompilationCache =
    open Fable.AST.Fable
    type Message =
        | Update of Parsing.Message
        | GetEntity of EntityRef * AsyncReplyChannel<Entity>
        | TryGetEntity of EntityRef * AsyncReplyChannel<Entity option>
        | GetMember of MemberRef * AsyncReplyChannel<MemberFunctionOrValue>
        | TryGetMember of MemberRef * AsyncReplyChannel<MemberFunctionOrValue option>
        // | QueueSize of AsyncReplyChannel<int>
open CompilationCache
let inline print_debug (s: string) = ()
let refKey ref =
    match ref with
    | MemberRef(declaringEntity, info) ->
        declaringEntity.FullName + "." + info.CompiledName
    | GeneratedMemberRef generatedMember ->
        defaultArg (generatedMember.Info.DeclaringEntity |> Option.map (fun ent -> ent.FullName)) "" +
            "." + generatedMember.Info.Name
type FableCompilationCache() as this =
    // class
    let mutable files = Map.empty
    let mutable entities = Map.empty
    let mutable entitiesByRef = Map.empty
    let mutable membersByRef = Map.empty
    let mutable members = Map.empty
    let mutable entityRefs = Map.empty
    let mutable running = true
    let agent = MailboxProcessor<CompilationCache.Message>.Start(fun inbox -> async {
        let compiler = this :> AST.Type.ICompiler
        while running do
            let! msg = inbox.Receive()
            match msg with
            | Update update ->
                print_debug "\nReceived update"
                match update with
                | Parsing.AddEntity(file, entity) ->
                    print_debug $"Entity: {entity.FullName}"
                    entities <- entities.Add(entity.FullName, (entity, file))
                    entitiesByRef <- entitiesByRef.Add(entity.Ref, entity)
                    Transforms.compiler.AddEntity entity
                | Parsing.AddMember(ref, file, entity, memberFunctionOrValue, memberDecl) ->
                    print_debug $"Member Entity: {entity.FullName}"
                    // todo: should this add entities?
                    print_debug $"todo: should this add entities?"
                    print_debug $"Identity: {memberDecl.Name}"
                    members <- members.Add ((entity.FullName, memberDecl.Name), (entity, file, memberDecl, memberFunctionOrValue))
                    // todo:
                    membersByRef <- membersByRef.Add (refKey ref, (entity, file, memberDecl, memberFunctionOrValue))
                    Transforms.compiler.AddMember memberFunctionOrValue
                    Transforms.compiler.AddEntity entity
                | Parsing.AddMemberWithoutEntity(file, entityRef, memberRefInfo, memberDecl) ->
                    #if !FABLE_COMPILER
                    // todo: polyfill
                    System.Diagnostics.Debugger.Break()
                    #endif
                    // printHeader "Missing ent"
                    print_debug $"%A{entityRef}"
                    entityRefs <- entityRefs.Add(entityRef.FullName, (entityRef, file))
            | GetEntity (ref, channel) -> channel.Reply <| compiler.GetEntity ref
            | TryGetEntity (ref, channel) -> channel.Reply <| compiler.TryGetEntity ref
            | GetMember (ref, channel) -> channel.Reply <| compiler.GetMember ref
            | TryGetMember (ref, channel) -> channel.Reply <| compiler.TryGetMember ref
            // | QueueSize channel -> channel.Reply(inbox.CurrentQueueLength)
    })
    member val n : int = 0 with get, set
    member this.Update(update: Parsing.Message) =
        agent.Post (Update update)
    // member this.QueueSize =
        // agent.PostAndReply QueueSize
    override this.Finalize() =
        running <- false
    interface AST.Type.ICompiler with
        member this.GetEntity(ref) =
            try entitiesByRef[ref]
            with ex ->
                try entitiesByRef[{ ref with FullName = "Fable." + ref.FullName }]
                with ex ->
                    print_debug $"Error while searching for entity with reference %A{ref}"
                    Unchecked.defaultof<_>
        member this.GetMember(ref) =
            let ref = refKey ref
            try
                let (_, _, _, item) = membersByRef[ref]
                item
            with ex ->
                print_debug $"Error while searching for ref %A{ref}"
                Unchecked.defaultof<_>
        member this.TryGetEntity(ref) =
            entitiesByRef.TryFind ref
            |> Option.orElseWith (fun () -> entities.TryFind ("Fable." + ref.FullName) |> Option.map fst)
        member this.TryGetEntityWithName name =
            entities |> Map.tryFind name
        member this.TryGetMemberWithName (entityName: string, memberName: string) =
           members.TryFind (entityName, memberName)
        member this.TryGetMemberByRef (entityName: string, memberName: string) =
           membersByRef.TryFind (entityName + "." + memberName)
        member this.TryGetMember(ref) =
            membersByRef.TryFind (refKey ref)
            |> Option.map (fun (_, _, _, item) -> item)
        member this.SaveFile (com: Fable.Compiler) file =
            let addMethod (decl: Fable.AST.Fable.MemberDecl) =
                match decl.MemberRef with
                | MemberRef(declaringEntity, info) ->
                    let ent = com.GetEntity declaringEntity
                    let mem = ent.TryFindMember info
                    this.Update (Parsing.AddMember (decl.MemberRef, file, ent, mem.Value, decl))
                | _ -> ()
            for decl in file.Declarations do
                match decl with
                | Fable.AST.Fable.ClassDeclaration decl ->
                    this.Update (Parsing.AddEntity (file, com.GetEntity decl.Entity))
                    match decl.Constructor with
                    | Some ctor -> addMethod ctor
                    | None -> ()
                | Fable.AST.Fable.MemberDeclaration decl ->
                    addMethod decl
                | _ -> ()
    // end
open Fable.AST
open Fable.Transforms.State
let populateCache (cache: FableCompilationCache) (com: CompilerImpl) (file: Fable.File) =
    let addMethod (decl: MemberDecl) =
        match decl.MemberRef with
        | MemberRef(declaringEntity, info) ->
            let ent = com.GetEntity declaringEntity
            let mem = ent.TryFindMember info
            cache.Update (Parsing.AddMember (decl.MemberRef, file, ent, mem.Value, decl))
        | _ -> ()
    for decl in file.Declarations do
        match decl with
        | ClassDeclaration decl ->
            cache.Update (Parsing.AddEntity (file, com.GetEntity decl.Entity))
            match decl.Constructor with
            | Some ctor -> addMethod ctor
            | None -> ()
        | MemberDeclaration decl ->
            addMethod decl
        | _ -> ()

type FableCompilationCache with
    member this.UpdateWithFile com file = populateCache this com file
