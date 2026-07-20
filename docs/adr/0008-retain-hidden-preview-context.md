# ADR 0008: Retain hidden preview context

- Status: Accepted
- Date: 2026-07-21
- Amends: ADR 0007

## Context

ADR 0007 preserves the current documentation path during live rebuilds, but
leaves `retainContextWhenHidden` disabled. VS Code can therefore discard the
WebView document after the reader selects another editor tab. Recreating the
document loads the configured preview URL and returns the reader to the first
page.

The wrapper cannot restore the current page after disposal without learning
the nested cross-origin frame's location or adding navigation tracking to the
generated site. Those alternatives expand the preview protocol and weaken the
privacy and isolation properties established by ADR 0007.

The Web document visibility API is not a reliable editor-tab signal. A retained
WebView stays loaded when VS Code hides its editor, and in an Insiders 1.130
measurement its document remained `visible` and emitted no `visibilitychange`
event. The extension did receive `WebviewPanel.onDidChangeViewState` with
`panel.visible` changing from true to false and back to true.

## Decision

Preview panels enable `retainContextWhenHidden`. VS Code keeps the wrapper,
bridge, and document frames alive while a preview panel is hidden, preserving
the current path, query, fragment, scroll position, and other in-page state
when the reader switches between editor tabs.

The wrapper announces readiness with its per-installation channel token. The
extension answers with the current `panel.visible` value and sends each later
`onDidChangeViewState` value over the same token-bound message channel. The
wrapper does not use the Web document visibility API for this decision.

When a bridge handshake timeout expires while the extension reports the panel
hidden, the wrapper leaves the timer disarmed instead of entering the permanent
legacy fallback. When the extension reports the panel visible, the wrapper
sends another bridge hello and starts a fresh timeout. An explicit bridge
rejection or failure to acquire the VS Code API still selects the legacy
fallback immediately, and a handshake that times out while the panel is
reported visible still selects it after the timeout.

Before the first token-matched visibility response arrives, the wrapper treats
the panel as visible. Failure to synchronize that initial state within the
bounded timeout therefore selects the legacy fallback even if the panel was
actually hidden. This fail-safe avoids an indefinitely blank preview when the
extension-to-WebView message path is unavailable, at the cost of a small
initial downgrade window.

ADR 0007 allowed the wrapper to acknowledge a refresh as soon as it queued the
message for an inactive bridge. The wrapper now acknowledges only after it has
forwarded the refresh to an active bridge or started the legacy frame reload.
If neither happens within the extension acknowledgement timeout, the extension
replaces the wrapper HTML through its existing compatibility fallback.
Only the latest refresh is retained while the bridge is inactive. A newer
refresh replaces the pending one; the superseded attempt remains
unacknowledged and follows the same bounded extension fallback.

Closing the preview panel still disposes its context. Status transitions,
preview-origin changes, and legacy HTML-replacement refreshes may still replace
the document as described by ADR 0007.

## Consequences

- Returning to an open preview tab keeps the page the reader was viewing.
- Hidden preview panels consume WebView memory until they are closed or the
  extension is deactivated.
- A handshake delayed while the panel is hidden gets a fresh attempt when the
  panel becomes visible instead of permanently downgrading that panel.
- A refresh cannot remain indefinitely queued behind an inactive bridge while
  being reported to the extension as accepted.
- The preview does not need to observe or persist the generated document's URL.
