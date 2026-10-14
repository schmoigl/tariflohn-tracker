// Uploads dist/ to the web server, then deletes files there that are no longer part of the build
// (e.g. data files with old content hashes). `node deploy.js --dry-run` only lists what would be deleted.
import {spawnSync} from "node:child_process";
import {existsSync, readdirSync} from "node:fs";
import {join, relative, sep} from "node:path";

const host = "lschmoigl@data-science.wifo.ac.at";
const dir = "/home/lschmoigl/datascience/htdocs/tariflohntracker";
const dryRun = process.argv.includes("--dry-run");

// Prefer Git's ssh/scp on Windows: Windows' own OpenSSH rejects the key in ~/.ssh because of its
// file permissions and then hangs at a password prompt. BatchMode makes a failed login fail fast.
const gitBin = "C:/Program Files/Git/usr/bin";
const tool = (name) => (process.platform === "win32" && existsSync(`${gitBin}/${name}.exe`) ? `${gitBin}/${name}.exe` : name);
const sshOptions = ["-o", "BatchMode=yes"];

function run(command, args, input) {
  const result = spawnSync(command, args, {input, stdio: [input === undefined ? "inherit" : "pipe", "inherit", "inherit"]});
  if (result.status !== 0) throw new Error(`${command} failed: ${result.error ?? `exit code ${result.status}`}`);
}

const files = readdirSync("dist", {recursive: true, withFileTypes: true})
  .filter((entry) => entry.isFile())
  .map((entry) => relative("dist", join(entry.parentPath, entry.name)).split(sep).join("/"));

// Never prune against an incomplete build: that would delete the live site.
if (!files.includes("index.html") || !files.includes("en/index.html")) {
  throw new Error("dist/ is incomplete (index.html or en/index.html missing); run `observable build` first");
}

if (!dryRun) {
  run(tool("scp"), [...sshOptions, "-r", ...readdirSync("dist").map((name) => `dist/${name}`), `${host}:${dir}/`]);
}

// The list of built files is sent on stdin; files on the server that are not in it get removed.
const remove = dryRun ? 'echo "would delete: $f"' : 'rm -v -- "$f"';
run(
  tool("ssh"),
  [
    ...sshOptions,
    host,
    `set -e
cd '${dir}'
keep=$(mktemp)
sort > "$keep"
find . -type f -printf '%P\\n' | sort | comm -23 - "$keep" | while IFS= read -r f; do ${remove}; done
${dryRun ? "" : "find . -mindepth 1 -type d -empty -delete"}
rm -f "$keep"`
  ],
  files.join("\n") + "\n"
);
