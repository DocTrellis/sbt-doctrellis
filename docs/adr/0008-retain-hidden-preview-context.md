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

## Decision

Preview panels enable `retainContextWhenHidden`. VS Code keeps the wrapper,
bridge, and document frames alive while a preview panel is hidden, preserving
the current path, query, fragment, scroll position, and other in-page state
when the reader switches between editor tabs.

The wrapper does not enter the permanent legacy fallback when its initial
bridge handshake timeout expires while the panel is hidden. When the panel
becomes visible, it sends another bridge hello and starts a fresh bounded
timeout. An explicit bridge rejection or failure to acquire the VS Code API
still selects the legacy fallback immediately, and a handshake that times out
while visible still selects it after the timeout.

Closing the preview panel still disposes its context. Status transitions,
preview-origin changes, and legacy HTML-replacement refreshes may still replace
the document as described by ADR 0007.

## Consequences

- Returning to an open preview tab keeps the page the reader was viewing.
- Hidden preview panels consume WebView memory until they are closed or the
  extension is deactivated.
- A handshake delayed while the panel is hidden gets a fresh attempt when the
  panel becomes visible instead of permanently downgrading that panel.
- The preview does not need to observe or persist the generated document's URL.
