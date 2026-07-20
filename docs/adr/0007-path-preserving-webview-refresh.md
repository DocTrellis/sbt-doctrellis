# ADR 0007: Path-preserving WebView refresh

- Status: Accepted
- Date: 2026-07-20
- Amends: ADR 0003, ADR 0006

## Context

ADR 0006 refreshes a VS Code preview after publication by assigning new wrapper
HTML. That recreates the preview iframe at the announced root URL. A reader on
a nested page therefore returns to the first page after every successful
documentation rebuild.

The WebView cannot read the URL of its existing external iframe because the
same-origin policy and empty sandbox intentionally isolate generated
documentation. Allowing generated page scripts or asking them to report their
location would weaken that boundary and would make live preview depend on
content injected into the published site.

## Decision

The preview HTTP server reserves `/.doctrellis/webview-bridge` for a fixed
DocTrellis-owned bridge document. The response takes precedence over generated
site files at that path, is never cached, and has a default-deny CSP whose
script and style directives contain only hashes of its fixed inline assets. It
also identifies itself with `X-DocTrellis-Bridge: 1`. The extension accepts a
bridge only for an exact HTTP 200 response carrying that exact marker; redirects
and ordinary successful pages are not bridge capabilities. The extension
attempts this probe only for a version 2 session that has a publication-event
stream. Fixed-URL version 1 previews never receive the broader iframe sandbox.

The extension caches the bridge result by workspace and preview URL for the
current session. A lifecycle transition that starts or ends a session changes
the cache generation, so an older in-flight probe cannot populate a newer
session. After verification, the extension resolves both the bridge URL and
the announced preview URL independently through `asExternalUri`. It uses the
bridge only when both resolved URLs have the same embeddable origin.

The VS Code wrapper enables scripts but its CSP likewise permits only one
fixed, hash-authorized script. It embeds the bridge with
`sandbox="allow-scripts allow-same-origin"`. The bridge creates a second iframe
for the generated documentation with only `allow-same-origin`; generated page
scripts, forms, popups, and top-level navigation remain disabled. Because the
bridge and document frame have the same resolved origin, the trusted bridge
can invoke `location.reload()` on the document frame without reading or
reporting its URL.

On a `siteChanged` event, the extension sends the accepted revision to the
existing WebView with `postMessage`. The wrapper forwards it to the bridge,
which reloads the document frame at its current URL. Path, query, and fragment
therefore survive a successful rebuild. The server still sends
`Cache-Control: no-store`, so the reload reads the newly published files.
The wrapper targets messages at the verified bridge origin and accepts bridge
responses only from that frame and origin. The bridge learns the WebView parent
origin from an origin-bound hello and replies only to that origin. The wrapper
accepts extension refresh messages only from its exact VS Code parent; those
messages additionally carry a per-installation random channel token that is
not forwarded to the bridge. Each attempt also carries a random refresh ID.
After accepting and scheduling a refresh, the wrapper acknowledges the
matching refresh ID, revision, and token through the VS Code API.
The extension treats transport delivery without that acknowledgement as a
failure and replaces the wrapper HTML. The acknowledgement means that the
wrapper accepted and scheduled the refresh, not that the bridge already
applied it; a bridge that never becomes active still reaches the wrapper's
bounded legacy fallback. Messages contain no page content or reported
navigation location; the resolved preview URL is sent only to the verified
bridge origin during initialization.

This amends ADR 0006's requirement that every accepted revision appear in newly
assigned inert HTML. A successful bridge refresh deliberately leaves the
installed wrapper HTML, including its original `data-doctrellis-revision`,
unchanged; the revision is instead carried by the parent- and token-validated
extension-to-WebView message. HTML replacement fallbacks still include the new
revision.

If the bridge endpoint is absent, cannot be externally mapped to the preview
origin, rejects initialization, or does not complete its handshake, the
wrapper falls back to the ADR 0006 direct empty-sandbox iframe. If VS Code
cannot deliver a refresh message or the wrapper does not acknowledge handling
it within the bounded timeout, the extension falls back to replacing the
wrapper HTML. These compatibility paths still refresh the content but may
return it to the preview root. This preserves compatibility with older plugins
and arbitrary fixed-URL preview commands without treating their content as a
trusted bridge.

Lifecycle states continue to replace preview content with escaped status
markup. Path preservation applies to successful in-session publications, not
to a stopped process, a changed preview origin, a panel the user closed, or a
hidden panel whose WebView context VS Code discarded. The extension keeps
`retainContextWhenHidden` disabled to avoid retaining every hidden preview's
document and bridge frames in memory.

## Consequences

- Successful version 2 rebuilds keep the page path, query, and fragment being
  viewed in an open panel.
- Generated documentation remains script-disabled inside the WebView; only
  the two fixed bridge scripts can execute.
- A successful status code alone never grants the broader bridge sandbox, and
  version 1 fixed-URL previews retain the direct empty sandbox.
- The bridge uses a same-origin nested frame, so a remote URI mapping that
  assigns different origins to the root and reserved path uses the legacy
  refresh instead.
- A generated site cannot claim the reserved bridge path while served by
  `doctrellisPreview`.
- The bridge HTTP response, CSP hashes, message fallback, and registry ordering
  require regression coverage in both the plugin and extension suites.

## Alternatives considered

- Reading the direct iframe location from the VS Code wrapper remains
  impossible across origins.
- Enabling scripts in generated documentation and injecting a navigation
  reporter was rejected because preview behavior would modify and trust site
  content.
- Tracking document requests in the HTTP server was rejected because assets,
  redirects, multiple VS Code windows, and external browsers make the latest
  requested path an unreliable panel-navigation identity.
- Adding server-sent events to every preview page was rejected because the
  authenticated IPC publication event already provides the correct ordering.
