

// // let fs: any = {};
// let filesToRead: string[] = [
//     "Fable.Tests.C/Fable.Tests.C.fsproj",
//     "Fable.Tests.C/System.fs",
//     "Fable.Tests.C/Program.fs",
//     // "fable-metadata/lib/Fable.Core.dll"
// ]

// await (async () => {
//     for (let file of filesToRead) {
//         let text = await (await fetch(file)).text();
//         fs.writeFileSync(file, text);
//     }
// })();
// fs.readFileAsync = async (path: string) => {
//     let response = await fetch(path);
//     if (path.endsWith('.dll')) {
//         let blob = await response.blob()
//         console.log(blob)
//         let bytes = await blob.arrayBuffer();
//         let data = []
//         for (let i = 0; i < bytes.byteLength; i++) {
//             data.push(i)
//         }
//         console.log(bytes);
//         console.log(data);
//         return data;
//     }
//     else {
//         let text = await response.text();
//         return text;
//     }
// }
// let files: any = {}
// let populateFileDb = async () => {
//     for (let file of filesToRead) {
//         files[file] = await fs.readFileAsync(file);
//     }
// }
// await populateFileDb();
// const prefix = '/home/dave/repos/fable2/src/fable-standalone/test/bench-compiler/../../../'
// fs.readFileSync = (path: string, type: string) => {
//     let p = path;
//     if (path.startsWith(prefix)) {
//         p = path.substring(prefix.length);
//     }
//     console.log(p);
//     let file = files[p];
//     console.log(file)
//     if (file === undefined) {
//         filesToRead.push(p);
//         console.log(filesToRead);
//     }
//     // if (type != "utf8")
//         // return {};
//     return file;
//     // if (file === undefined) {
//     //     throw path;
//     // }
//     // fs.readFileAsync(path);
// }
// window.fs = fs;
window.path = {
    resolve: (args) => {
        console.log(args);
        return args;
    }
}
window.os = {
    homedir: (args) => {
        // console.log(args);
        return '';
    }
}
window.proc = {
    hrtime: (args) => {
        if (args === undefined) { return [ 0, Date.now()] ; }
        else { return [ 0, Date.now() - args[1] ]; }
    }
}
window.util = {
    existsSync: (file) => { return true; },
    ensureDirExists: (dir) => {}
}
let app = null;
window.initFable = (async () => { 
    let fs = await import('./JS_fs'); //* as fs from './JS_fs'
    window.fs = fs;
    let projectText = `
    <Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net8.0</TargetFramework>
    </PropertyGroup>

    <ItemGroup>
        <Compile Include="System.fs" />
        <Compile Include="Program.fs" />
    </ItemGroup>

    </Project>`
    let programText = 'let main () = printfn $"{1337}"'
    let systemFs = `
module Fable

module Hash =
    let hashValue (pointer: nativeint) (length: int) : int =
        let pointer: nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt pointer
        let mutable hash = 420
        let M = 69
        for i in 0..length - 1 do
            let value = NativeInterop.NativePtr.get pointer i
            hash <- (M * hash) + int value
        if hash < 0 then -1 * hash else hash
module System =
    module Collections =
        module Generic =
            type List<'t>() =
                let mutable items: 't[] = Array.zeroCreate 0
                let mutable n = 0
                member this.Add(item: 't) =
                    if n >= items.Length then
                        let copy = items
                        items <- Array.zeroCreate ((items.Length + 1) * 2)
                        for i in 0..n - 1 do
                            items[i] <- copy[i]
                    items[n] <- item
                    n <- n + 1
                    printfn $"[{n}] ==> {item}"
                member this.Item
                    with get index = items[index]
                member this.Count = n
            type Dictionary<'key, 'value when 'key: equality>() =
                // let items: 'value[] = Array.zeroCreate 0
                let buckets: ('key * 'value)[][] = Array.zeroCreate 8
                let counts: int[] = Array.zeroCreate 8
                do
                    for i in 0..7 do
                        buckets[i] <- Array.zeroCreate 1
                        counts[i] <- 0
                // let keys: 'key[][] = Array.zeroCreate 0
                // let buckets: 'value[][] = Array.zeroCreate 0
                member this.Item
                    with get (key: 'key) : 'value =
                        let hash = key.GetHashCode()
                        let bucketIndex = hash % buckets.Length
                        let mutable found = false
                        let mutable index = 0
                        while not found && index < counts[bucketIndex] do
                            let value: 'key = fst (buckets[bucketIndex][index])
                            // let value: 'key = keys[bucketIndex][index]
                            if value = key then
                                found <- true
                            index <- index + 1
                        if not found then
                            exit 1337
                            // todo: exit 1337
                        snd (buckets[bucketIndex][index - 1])
                member this.Add (key: 'key, value: 'value) =
                    // let mutable k = key
                    // let hash = Hash.hashValue (NativeInterop.NativePtr.toNativeInt &&k) sizeof<'key>
                    let hash = key.GetHashCode()
                    printfn $"hash = {hash}"
                    printfn $"hash index = {hash % buckets.Length}"
                    let bucketIndex = hash % buckets.Length
                    if counts[bucketIndex] >= buckets[bucketIndex].Length then
                        let bucket = buckets[bucketIndex]
                        let newBucket = Array.zeroCreate (bucket.Length * 2)
                        for i in 0..(counts[bucketIndex]) - 1 do
                            newBucket[i] <- bucket[i]
                        buckets[bucketIndex] <- newBucket
                    buckets.[bucketIndex].[counts[bucketIndex]] <- (key, value)
                    counts[bucketIndex] <- counts[bucketIndex] + 1
`
    app = await import('./app.fs.js')
    window.run = app.runMain;
    await fs.initFs();
    window.fs = fs;
    fs.writeFileSync('Program.fsproj', projectText)
    fs.writeFileSync('System.fs', systemFs);
    fs.writeFileSync('Program.fs', programText);
    // setTimeout(async () => {
    //     // let resp = await fetch('bench-compiler/Fable.Tests.C/System.fs');
    //     // let system_fs = await resp.text()
    //     // console.log(system_fs)
    //     // app.runMain(['Program.fsproj', '--language', 'c'], console.log)
    // }, 2000);
    window.build = (text, callback) => {
        fs.writeFileSync('Program.fs', text);
        app.runMain(['Program.fsproj', '--language', 'c'], callback)
    }
    // let response = app.runMain(['Fable.Tests.C/Fable.Tests.C.fsproj', '--printAst', '--language', 'c'], (data) => {
    //     console.log(data)
    //     // document.body.innerHTML = '<code style="white-space: pre-wrap;">' + data + '</code>'
    // })
});
//();

// let app = await import('./app.fs.js')
// console.log(app.JS_fs);
// console.log(response);
// function run(args) {
    // return app.runMain;
// }
// console.log(filesToRead);
export { app }
// export { run: app.runMain };