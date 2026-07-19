import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const manifestUrl = new URL("../package.json", import.meta.url);
const manifest = JSON.parse(await readFile(manifestUrl, "utf8"));
const commands = new Map(
  manifest.contributes.commands.map((command) => [command.command, command])
);

assert.ok(
  manifest.activationEvents.includes("onCommand:doctrellis.openPreview")
);
assert.ok(
  manifest.activationEvents.includes(
    "onCommand:doctrellis.openExternalPreview"
  )
);
assert.ok(commands.has("doctrellis.openPreview"));
assert.equal(
  commands.get("doctrellis.openExternalPreview")?.title,
  "DocTrellis: Open in External Browser"
);
assert.equal(
  commands.get("doctrellis.openPreview")?.enablement,
  "isWorkspaceTrusted"
);
assert.equal(
  commands.get("doctrellis.openExternalPreview")?.enablement,
  "isWorkspaceTrusted"
);
