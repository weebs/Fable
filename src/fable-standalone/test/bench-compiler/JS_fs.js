"use strict";
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
].map((a) => a + ".dll");
function fetchBlob(getUrl, name) {
  return fetch(getUrl(name)).then(function(res) {
    if (res.ok) {
      return res.arrayBuffer().then((b) => {
        return [name, new Uint8Array(b)];
      });
    } else {
      throw new Error("[ASSEMBLY LOAD] " + res.status + ": " + res.statusText);
    }
  });
}
function getAssemblyReader(getUrl, assemblies2) {
  return Promise.all(assemblies2.map((name) => fetchBlob(getUrl, name))).then(function(kvs) {
    var metadata = /* @__PURE__ */ new Map();
    for (var kv of kvs) {
      metadata.set(kv[0] + ".dll", kv[1]);
    }
    return (name) => metadata.get(name);
  });
}
function escapeJsStringLiteral(str) {
  return JSON.stringify(str).slice(1, -1).replace(/\u2028/g, "\\u2028").replace(/\u2029/g, "\\u2029");
}
let fs = new Map(await Promise.all(assemblies.map((a) => fetchBlob((url) => "fable-metadata/lib/" + url, a))));
let callbacks = [];
let fileWatchers = /* @__PURE__ */ new Map();
let JS_fs = {
  // Promise.all(assemblies.map(a => fetchBlob(url => 'dll/' + url, a)))
  // .then(files => {
  // const fs = new Map(files)
  // window.JS_fs = { 
  //
  // })
  writeFileSync: (name, contents) => {
    fs.set(name, contents);
    try {
      for (const callback of callbacks) {
        callback([name, contents]);
      }
      for (const callback of fileWatchers.get(name) ?? []) {
        callback([name, contents]);
      }
    } catch (e) {
      console.log(e);
    }
  },
  // fs.readFileSync = (path: string, type: string) => {
  //     let p = path;
  //     if (path.startsWith(prefix)) {
  //         p = path.substring(prefix.length);
  //     }
  readFileSync: (_name, type) => {
    const prefix = "/home/dave/repos/fable2/src/fable-standalone/test/bench-compiler/../../../";
    let name = _name;
    if (name.startsWith(prefix)) {
      name = name.substring(prefix.length);
    }
    console.log(name, type);
    name = name.replace("\\", "/");
    let path = name.replace("\\", "/").split("/");
    let filename = path[path.length - 1];
    if (!fs.has(filename) && !fs.has(name))
      console.log("Missing file: ", name);
    return fs.get(name) ?? fs.get(filename);
  },
  watchFile: (name, callback) => {
    if (!fileWatchers.has(name)) {
      fileWatchers.set(name, []);
    }
    fileWatchers.get(name).push(callback);
  },
  allFiles: (callback) => {
    let add = true;
    for (const c of callbacks) {
      if (c === callback) {
        add = false;
        break;
      }
    }
    if (add) {
      console.log("adding callback");
      callbacks.push(callback);
    }
    return [...fs.keys()];
  }
};
export const allFiles = JS_fs.allFiles;
export const writeFileSync = JS_fs.writeFileSync;
export const watchFile = JS_fs.watchFile;
export const readFileSync = JS_fs.readFileSync;
window.files = fs;
export const files = fs;
