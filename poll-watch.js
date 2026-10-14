// Preloaded by `npm run dev`: replaces fs.watch with stat polling. On network shares (K: is
// \\HFUSR\...) fs.watch fails with "UNKNOWN: unknown error, watch", which crashes the preview server.
import {EventEmitter} from "node:events";
import fs from "node:fs";
import {syncBuiltinESMExports} from "node:module";
import {basename} from "node:path";

const interval = 500;

fs.watch = function watch(path, options, listener) {
  if (typeof options === "function") listener = options;
  fs.statSync(path); // throw ENOENT for missing paths, like fs.watch
  const watcher = new EventEmitter();
  if (listener) watcher.on("change", listener);
  const poll = (curr, prev) => {
    if (curr.mtimeMs === prev.mtimeMs && curr.size === prev.size) return;
    watcher.emit("change", curr.mtimeMs === 0 ? "rename" : "change", basename(String(path)));
  };
  fs.watchFile(path, {interval, persistent: false}, poll);
  watcher.close = () => fs.unwatchFile(path, poll);
  watcher.ref = watcher.unref = () => watcher;
  return watcher;
};

// Update the named export so `import {watch} from "node:fs"` sees the replacement.
syncBuiltinESMExports();
