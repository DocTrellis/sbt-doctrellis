package org.doctrellis.vscode.infrastructure

import scala.collection.mutable
import scala.scalajs.js

import munit.FunSuite

class PreviewWebviewScriptSuite extends FunSuite:
  test("announces wrapper readiness with its channel token"):
    val harness = WrapperScriptHarness()

    assertEquals(
      harness.vscodeMessageTypes,
      List("doctrellis.preview.ready"),
    )
    assertEquals(harness.wrapperReadyTokens, List("channel-secret"))

  test("retries a hidden bridge handshake when the panel becomes visible"):
    val harness = WrapperScriptHarness()

    harness.fireFrameLoad()
    assertEquals(harness.bridgeMessageTypes, List("doctrellis.bridge.hello"))

    harness.sendVisibility(visible = false)
    harness.sendRefresh("attempt-1", "2")
    harness.fireNextTimeout()

    assertEquals(harness.frameUrl, harness.bridgeUrl)
    assertEquals(harness.refreshAcknowledgements, Nil)
    assertEquals(harness.pendingTimerCount, 0)
    assertEquals(
      harness.vscodeMessageTypes,
      List("doctrellis.preview.ready", "doctrellis.preview.ready"),
    )
    assertEquals(
      harness.wrapperReadyTokens,
      List("channel-secret", "channel-secret"),
    )

    harness.sendVisibility(visible = true)
    assertEquals(
      harness.bridgeMessageTypes,
      List("doctrellis.bridge.hello", "doctrellis.bridge.hello"),
    )
    assertEquals(harness.pendingTimerCount, 1)

    harness.sendBridgeActive()
    assertEquals(
      harness.bridgeMessageTypes,
      List(
        "doctrellis.bridge.hello",
        "doctrellis.bridge.hello",
        "doctrellis.preview.refresh",
      ),
    )
    assertEquals(harness.refreshAcknowledgements, List("attempt-1"))

  test("acknowledges a queued refresh only after the legacy reload starts"):
    val harness = WrapperScriptHarness()

    harness.sendRefresh("attempt-1", "2")
    assertEquals(harness.refreshAcknowledgements, Nil)

    harness.fireNextTimeout()
    assertEquals(harness.frameUrl, harness.directUrl)
    assertEquals(harness.refreshAcknowledgements, List("attempt-1"))

  test("retries the bridge after an initial visible fallback"):
    val harness = WrapperScriptHarness()

    harness.fireNextTimeout()
    assertEquals(harness.frameUrl, harness.directUrl)

    harness.sendRefresh("attempt-1", "2")
    assertEquals(harness.frameUrl, harness.bridgeUrl)
    assertEquals(harness.refreshAcknowledgements, Nil)
    assertEquals(harness.pendingTimerCount, 1)

    harness.fireFrameLoad()
    assertEquals(
      harness.bridgeMessageTypes,
      List("doctrellis.bridge.hello"),
    )

    harness.sendBridgeActive()
    assertEquals(
      harness.bridgeMessageTypes,
      List("doctrellis.bridge.hello", "doctrellis.preview.refresh"),
    )
    assertEquals(harness.refreshAcknowledgements, List("attempt-1"))

  test("ignores panel visibility messages with another channel token"):
    val harness = WrapperScriptHarness()

    harness.sendVisibility(visible = false, channelToken = "other")
    harness.fireNextTimeout()

    assertEquals(harness.frameUrl, harness.directUrl)

  test("accepts a token-bound host refresh without assuming its source"):
    val harness = WrapperScriptHarness()

    harness.sendBridgeActive()
    harness.sendRefresh(
      "attempt-1",
      "2",
    )

    assertEquals(
      harness.bridgeMessageTypes,
      List("doctrellis.preview.refresh"),
    )
    assertEquals(harness.refreshAcknowledgements, List("attempt-1"))
    assertEquals(harness.frameUrl, harness.bridgeUrl)
    assertEquals(harness.pendingTimerCount, 0)

  test("rejects a host refresh with another channel token"):
    val harness = WrapperScriptHarness()

    harness.sendBridgeActive()
    harness.sendRefresh(
      "attempt-1",
      "2",
      channelToken = "other",
    )

    assertEquals(harness.bridgeMessageTypes, Nil)
    assertEquals(harness.refreshAcknowledgements, Nil)

  test("rejects a token-bound refresh from the preview origin"):
    val harness = WrapperScriptHarness()

    harness.sendBridgeActive()
    harness.sendRefresh(
      "attempt-1",
      "2",
      origin = harness.bridgeOrigin,
    )

    assertEquals(harness.bridgeMessageTypes, Nil)
    assertEquals(harness.refreshAcknowledgements, Nil)

  test("rejects a refresh message with another protocol"):
    val harness = WrapperScriptHarness()

    harness.sendBridgeActive()
    harness.sendRefresh(
      "attempt-1",
      "2",
      protocol = 2,
    )

    assertEquals(harness.bridgeMessageTypes, Nil)
    assertEquals(harness.refreshAcknowledgements, Nil)

  test("accepts bridge messages only from the bridge frame and origin"):
    val harness = WrapperScriptHarness()

    harness.sendRefresh(
      "attempt-1",
      "2",
    )
    harness.sendBridgeActive(origin = "https://attacker.example")
    harness.sendBridgeActive(source = js.Dynamic.literal())

    assertEquals(harness.bridgeMessageTypes, Nil)
    assertEquals(harness.refreshAcknowledgements, Nil)

    harness.sendBridgeActive()

    assertEquals(
      harness.bridgeMessageTypes,
      List("doctrellis.preview.refresh"),
    )
    assertEquals(harness.refreshAcknowledgements, List("attempt-1"))

  test("accepts token-bound host visibility without assuming its source"):
    val harness = WrapperScriptHarness()

    harness.sendVisibility(visible = false)
    harness.fireNextTimeout()

    assertEquals(harness.frameUrl, harness.bridgeUrl)
    assertEquals(harness.pendingTimerCount, 0)
    assertEquals(
      harness.wrapperReadyTokens,
      List("channel-secret", "channel-secret"),
    )

private final class WrapperScriptHarness:
  val directUrl = "https://preview.example/guide/current.html"
  val bridgeUrl = "https://preview.example/.doctrellis/webview-bridge"

  val bridgeOrigin                  = "https://preview.example"
  private val installedChannelToken = "channel-secret"
  private val extensionHostOrigin   = "vscode-webview://doctrellis-preview"
  private val bridgeMessages        = mutable.ListBuffer.empty[js.Dynamic]
  private val vscodeMessages        = mutable.ListBuffer.empty[js.Dynamic]
  private val timers    = mutable.LinkedHashMap.empty[Int, js.Function0[Unit]]
  private var nextTimer = 0
  private var frameLoadListener = Option.empty[js.Function0[Unit]]
  private var messageListener   = Option.empty[js.Function1[js.Dynamic, Unit]]

  private val extensionHostMessageSource = js.Dynamic.literal()
  private val contentWindow              = js.Dynamic.literal()
  contentWindow.updateDynamic("postMessage")(
    (
        (message: js.Dynamic, _: String) =>
          bridgeMessages += message
          (),
    ): js.Function2[js.Dynamic, String, Unit],
  )

  private val frame = js.Dynamic.literal(
    dataset = js.Dynamic.literal(
      doctrellisPreviewUrl = directUrl,
      doctrellisBridgeOrigin = bridgeOrigin,
      doctrellisChannelToken = installedChannelToken,
    ),
    contentWindow = contentWindow,
    src = bridgeUrl,
  )
  frame.updateDynamic("addEventListener")(
    (
        (eventType: String, listener: js.Function) =>
          if eventType == "load" then
            frameLoadListener = Some(listener.asInstanceOf[js.Function0[Unit]])
          (),
    ): js.Function2[String, js.Function, Unit],
  )
  frame.updateDynamic("setAttribute")(
    ((_: String, _: String) => ()): js.Function2[String, String, Unit],
  )
  frame.updateDynamic("getAttribute")(
    (
        (name: String) =>
          if name == "sandbox" then "allow-scripts allow-same-origin" else null,
    ): js.Function1[String, String],
  )
  frame.updateDynamic("removeAttribute")(
    ((_: String) => ()): js.Function1[String, Unit],
  )

  private val document = js.Dynamic.literal()
  document.updateDynamic("getElementById")(
    ((_: String) => frame): js.Function1[String, js.Dynamic],
  )

  private val vscode = js.Dynamic.literal()
  vscode.updateDynamic("postMessage")(
    ((message: js.Dynamic) =>
      vscodeMessages += message
      (),
    ): js.Function1[js.Dynamic, Unit],
  )

  private val acquireVsCodeApi =
    (() => vscode): js.Function0[js.Dynamic]
  private val setTimeout =
    (
        (callback: js.Function0[Unit], _: Double) =>
          nextTimer += 1
          timers.update(nextTimer, callback)
          nextTimer,
    ): js.Function2[js.Function0[Unit], Double, Int]
  private val clearTimeout =
    ((value: js.Any) =>
      if value != null && js.typeOf(value) == "number" then
        val _ = timers.remove(value.asInstanceOf[Int])
      (),
    ): js.Function1[js.Any, Unit]
  private val addEventListener =
    (
        (eventType: String, listener: js.Function) =>
          if eventType == "message" then
            messageListener = Some(
              listener.asInstanceOf[js.Function1[js.Dynamic, Unit]],
            )
          (),
    ): js.Function2[String, js.Function, Unit]

  private val script = js.Dynamic
    .newInstance(js.Dynamic.global.Function)(
      "document",
      "acquireVsCodeApi",
      "setTimeout",
      "clearTimeout",
      "addEventListener",
      PreviewWebviewHtml.Script,
    )
    .asInstanceOf[js.Dynamic]

  private val _ = script.call(
    js.undefined,
    document,
    acquireVsCodeApi,
    setTimeout,
    clearTimeout,
    addEventListener,
  )

  def frameUrl: String = frame.src.asInstanceOf[String]

  def bridgeMessageTypes: List[String] = messageTypes(bridgeMessages)

  def vscodeMessageTypes: List[String] = messageTypes(vscodeMessages)

  def wrapperReadyTokens: List[String] =
    vscodeMessages.toList.collect:
      case message
          if stringField(message, "type") == "doctrellis.preview.ready" =>
        stringField(message, "channelToken")

  def refreshAcknowledgements: List[String] =
    vscodeMessages.toList.collect:
      case message
          if stringField(message, "type") ==
            "doctrellis.preview.refresh-accepted" =>
        stringField(message, "refreshId")

  def pendingTimerCount: Int = timers.size

  def fireFrameLoad(): Unit =
    frameLoadListener.fold(missing("frame load listener was not installed"))(
      _(),
    )

  def fireNextTimeout(): Unit =
    timers.headOption.fold(missing("fallback timer was not armed")):
      case (id, callback) =>
        val _ = timers.remove(id)
        callback()

  def sendVisibility(
      visible: Boolean,
      channelToken: String = installedChannelToken,
  ): Unit =
    sendMessage(
      js.Dynamic.literal(
        data = js.Dynamic.literal(
          protocol = 1,
          `type` = "doctrellis.preview.visibility",
          visible = visible,
          channelToken = channelToken,
        ),
        source = extensionHostMessageSource,
        origin = extensionHostOrigin,
      ),
    )

  def sendRefresh(
      refreshId: String,
      revision: String,
      channelToken: String = installedChannelToken,
      protocol: Int = 1,
      origin: String = extensionHostOrigin,
  ): Unit =
    sendMessage(
      js.Dynamic.literal(
        data = js.Dynamic.literal(
          protocol = protocol,
          `type` = "doctrellis.preview.refresh",
          refreshId = refreshId,
          revision = revision,
          channelToken = channelToken,
        ),
        source = extensionHostMessageSource,
        origin = origin,
      ),
    )

  def sendBridgeActive(
      source: js.Any = contentWindow,
      origin: String = bridgeOrigin,
  ): Unit =
    sendMessage(
      js.Dynamic.literal(
        data = js.Dynamic.literal(
          protocol = 1,
          `type` = "doctrellis.bridge.active",
        ),
        source = source,
        origin = origin,
      ),
    )

  private def sendMessage(event: js.Dynamic): Unit =
    messageListener.fold(missing("message listener was not installed"))(
      _(event),
    )

  private def messageTypes(messages: mutable.ListBuffer[js.Dynamic]) =
    messages.toList.map(stringField(_, "type"))

  private def stringField(value: js.Dynamic, name: String): String =
    value.selectDynamic(name).asInstanceOf[String]

  private def missing(message: String): Nothing =
    throw AssertionError(message)

private object WrapperScriptHarness:
  def apply(): WrapperScriptHarness = new WrapperScriptHarness
