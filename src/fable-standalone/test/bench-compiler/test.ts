

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
import * as fs from './JS_fs'
window.fs = fs;
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
    await fs.initFs();
    app = await import('./app.fs.js')
    window.run = app.runMain;
    // setTimeout(async () => {
    fs.writeFileSync('Program.fsproj', projectText)
    let resp = await fetch('System.fs');
    let system_fs = await resp.text()
    console.log(system_fs)
    fs.writeFileSync('System.fs', system_fs);
    // fs.writeFileSync('Program.fs', system_fs + '\n' + programText)
    fs.writeFileSync('Program.fs', system_fs + '\n' + programText)
    console.log(programText)
        // app.runMain(['Program.fsproj', '--language', 'c'], console.log)
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
// let app = await import('./app.fs.js')
// console.log(app.JS_fs);
// console.log(response);
// function run(args) {
    // return app.runMain;
// }
// console.log(filesToRead);
export { app }
// export { run: app.runMain };