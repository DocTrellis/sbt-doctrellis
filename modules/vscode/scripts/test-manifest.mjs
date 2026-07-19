import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

const manifestUrl = new URL("../package.json", import.meta.url);
const manifest = JSON.parse(await readFile(manifestUrl, "utf8"));
const schemaUrl = new URL("../schema/doctrellis.schema.json", import.meta.url);
const schema = JSON.parse(await readFile(schemaUrl, "utf8"));
const commands = new Map(
  manifest.contributes.commands.map((command) => [command.command, command])
);

const expectedCommands = new Map([
  ["doctrellis.startPreview", ["DocTrellis: Start Preview", true]],
  ["doctrellis.stopPreview", ["DocTrellis: Stop Preview", false]],
  ["doctrellis.restartPreview", ["DocTrellis: Restart Preview", true]],
  ["doctrellis.openPreview", ["DocTrellis: Open Preview", true]],
  [
    "doctrellis.openExternalPreview",
    ["DocTrellis: Open in External Browser", true],
  ],
  ["doctrellis.showOutput", ["DocTrellis: Show Output", false]],
]);

assert.deepEqual(
  [...commands.keys()].sort(),
  [...expectedCommands.keys()].sort()
);
assert.deepEqual(
  [...manifest.activationEvents].sort(),
  [...expectedCommands.keys()].map((id) => `onCommand:${id}`).sort()
);

for (const [id, [title, requiresTrust]] of expectedCommands) {
  const command = commands.get(id);
  assert.equal(command?.title, title);
  assert.equal(
    command?.enablement,
    requiresTrust ? "isWorkspaceTrusted" : undefined
  );
}

assert.equal(manifest.capabilities.untrustedWorkspaces.supported, "limited");

const settings = manifest.contributes.configuration.properties;
assert.deepEqual(settings["doctrellis.openPreviewWhenReady"], {
  type: "boolean",
  default: true,
  description:
    "Open the preview inside VS Code after the configured URL becomes ready.",
});
assert.equal(settings["doctrellis.pollIntervalMs"].type, "integer");
assert.equal(settings["doctrellis.pollIntervalMs"].default, 500);
assert.equal(settings["doctrellis.pollIntervalMs"].minimum, 100);
assert.equal(settings["doctrellis.pollIntervalMs"].maximum, 10000);

assert.deepEqual(manifest.contributes.jsonValidation, [
  {
    fileMatch: "**/.doctrellis.json",
    url: "./schema/doctrellis.schema.json",
  },
]);

const commandPattern = new RegExp(schema.$defs.command.pattern);
const argumentPattern = new RegExp(schema.$defs.args.items.pattern);
const directoryPattern = new RegExp(schema.$defs.workingDirectory.pattern);
const urlPattern = new RegExp(
  schema.$defs.fixedPreview.properties.url.pattern
);

assert(commandPattern.test("sbt"));
assert(!commandPattern.test(" \t"));
assert(!commandPattern.test("sbt\u0000"));
assert(argumentPattern.test(""));
assert(!argumentPattern.test("preview\u0000"));
assert(directoryPattern.test("."));
assert(!directoryPattern.test(" \t"));
assert(!directoryPattern.test("docs\u0000"));
assert(urlPattern.test("http://localhost"));
assert(urlPattern.test("https://127.0.0.1:65535/docs"));
assert(urlPattern.test("http://127.0.0.1:00001"));
assert(!urlPattern.test("http://bad host:4242"));

for (let port = 0; port <= 99999; port += 1) {
  assert.equal(
    urlPattern.test(`http://localhost:${port}`),
    port >= 1 && port <= 65535,
    `unexpected schema result for TCP port ${port}`
  );
}
