import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const repositoryRoot = fileURLToPath(new URL("../../..", import.meta.url));
const stage = process.argv[2];
const task = {
  fast: "vscode / Compile / fastLinkJS",
  full: "vscode / Compile / fullLinkJS",
  test: ";vscodeDomain / testFull;vscodeApplication / testFull;vscode / testFull"
}[stage];

if (!task) {
  console.error(`Unknown build stage: ${stage}`);
  process.exit(2);
}

const result = spawnSync("sbt", ["--batch", task], {
  cwd: repositoryRoot,
  stdio: "inherit",
  shell: false
});

if (result.error) {
  console.error(result.error.message);
  process.exit(1);
}
process.exit(result.status ?? 1);
