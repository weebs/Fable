let assemblies = [
    "Fable.Core",
    "FSharp.Core",
    "mscorlib",
    "netstandard",
    "System.Collections",
    "System.Collections.Concurrent",
    "System.ComponentModel",
    "System.ComponentModel.Primitives",
    "System.ComponentModel.TypeConverter",
    "System.Console",
    "System.Core",
    "System.Diagnostics.Debug",
    "System.Diagnostics.Tools",
    "System.Diagnostics.Tracing",
    "System.Globalization",
    "System",
    "System.IO",
    "System.Net.Requests",
    "System.Net.WebClient",
    "System.Numerics",
    "System.Reflection",
    "System.Reflection.Extensions",
    "System.Reflection.Metadata",
    "System.Reflection.Primitives",
    "System.Reflection.TypeExtensions",
    "System.Runtime",
    "System.Runtime.Extensions",
    "System.Runtime.Numerics",
    "System.Runtime.InteropServices",
    "System.Text.Encoding",
    "System.Text.Encoding.Extensions",
    "System.Text.RegularExpressions",
    "System.Threading",
    "System.Threading.Tasks",
    "System.Threading.Thread",
    "System.ValueTuple"
].map(a => a + ".dll")
function fetchBlob(getUrl, name): Promise<[string, Uint8Array]> {
    return fetch(getUrl(name))
        .then(function (res) {
            if (res.ok) {
                return res.arrayBuffer().then(b => {
                    return [name, new Uint8Array(b)]
                });
            } else {
                throw new Error("[ASSEMBLY LOAD] " + res.status + ": " + res.statusText);
            }
        });
}

function getAssemblyReader(getUrl, assemblies) {
    return Promise.all(assemblies.map(name => fetchBlob(getUrl, name)))
        .then(function (kvs) {
            var metadata = new Map();
            for (var kv of kvs) {
                metadata.set(kv[0] + ".dll", kv[1]);
            }
            return (name) => metadata.get(name);
        });
}

// JSON.stringify doesn't escape line/paragraph separators, which are not valid in JS.
// https://github.com/expressjs/express/issues/1132
function escapeJsStringLiteral(str) {
  return JSON.stringify(str).slice(1, -1).replace(/\u2028/g, '\\u2028').replace(/\u2029/g, '\\u2029');
}
let fs: Map<string, Uint8Array | string> = new Map();
export const initFs = (async () => {
    console.log('grabbing files')
    const files = await Promise.all(assemblies.map(a => fetchBlob(url => 'fable-metadata/lib/' + url, a)));
    for (const [name, value] of files) {
        fs.set(name, value);
    }
});
let callbacks = []
let fileWatchers = new Map()
let JS_fs = {
// Promise.all(assemblies.map(a => fetchBlob(url => 'dll/' + url, a)))
// .then(files => {
    // const fs = new Map(files)
    // window.JS_fs = { 
//
// })
    writeFileSync: (name: string, contents: Uint8Array | string) => {
        fs.set(name, contents) // contents.replace('#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>', '#include "neslib.h"\n#define true 1\n#define false 0'))
        try {
            for (const callback of callbacks) {
                callback ([name, contents])
            }
            for (const callback of fileWatchers.get(name) ?? []) {
                callback ([name, contents])
            }
        } catch (e) { console.log(e) }
    },
// fs.readFileSync = (path: string, type: string) => {
//     let p = path;
//     if (path.startsWith(prefix)) {
//         p = path.substring(prefix.length);
//     }
    readFileSync: (_name, type?): Uint8Array | string | undefined => {
        const prefix = '/home/dave/projects/fable_github_demo/Fable/src/fable-standalone/test/bench-compiler/../../../'
        let name = _name;
        if (name.startsWith(prefix)) {
            name = name.substring(prefix.length);
        }
        console.log(name, type);
        name = name.replace('\\', '/')
        let path = name.replace('\\', '/').split('/')
        let filename = path[path.length - 1]
        if (!fs.has(filename) && !fs.has(name))
            console.log('Missing file: ', name)
        return fs.get(name) ?? fs.get(filename)
    },
    watchFile: (name: string, callback) => {
        if (!fileWatchers.has(name)) {
            fileWatchers.set(name, [])
        }
        fileWatchers.get(name).push(callback)
    },
    allFiles: (callback?: (_: [string, Uint8Array | string]) => void) => {
        let add = true;
        for (const c of callbacks) {
            if (c === callback) {
                add = false;
                break;
            }
        }
        if (add) { console.log('adding callback'); callbacks.push(callback) }
        return [...fs.keys()]
    }
}

export const allFiles = JS_fs.allFiles 
export const writeFileSync = JS_fs.writeFileSync 
export const watchFile = JS_fs.watchFile
export const readFileSync = JS_fs.readFileSync
window.files = fs;
export const files = fs;