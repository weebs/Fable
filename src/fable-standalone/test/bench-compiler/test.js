"use strict";
// let filesToRead = [
//   "Fable.Tests.C/Fable.Tests.C.fsproj",
//   "Fable.Tests.C/System.fs",
//   "Fable.Tests.C/Program.fs"
//   // "fable-metadata/lib/Fable.Core.dll"
// ];
// await (async () => {
//   for (let file of filesToRead) {
//     let text = await (await fetch(file)).text();
//     fs.writeFileSync(file, text);
//   }
// })();
import * as fs from "./JS_fs.js";
window.fs = fs;
window.path = {
  resolve: (args) => {
    console.log(args);
    return args;
  }
};
window.os = {
  homedir: (args) => {
    return "";
  }
};
window.proc = {
  hrtime: (args) => {
    if (args === void 0) {
      return [0, Date.now()];
    } else {
      return [0, Date.now() - args[1]];
    }
  }
};
window.util = {
  existsSync: (file) => {
    return true;
  },
  ensureDirExists: (dir) => {
  }
};
let app = await import("./app.fs.js");
let response = app.runMain(["Fable.Tests.C/Fable.Tests.C.fsproj", "--printAst", "--language", "c"], (data) => {
  console.log(data);
});
console.log(response);
function run(args) {
  return app.runMain;
}
export { run };
