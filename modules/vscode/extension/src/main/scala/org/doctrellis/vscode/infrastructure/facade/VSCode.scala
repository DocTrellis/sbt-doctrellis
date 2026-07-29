package org.doctrellis.vscode.infrastructure.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait Disposable extends js.Object:
  def dispose(): Unit = js.native

@js.native
trait ExtensionContext extends js.Object:
  val subscriptions: js.Array[Disposable] = js.native

@js.native
trait OutputChannel extends Disposable:
  def append(value: String): Unit                = js.native
  def show(preserveFocus: Boolean = false): Unit = js.native

@js.native
trait Webview extends js.Object:
  var html: String                                      = js.native
  def postMessage(message: js.Any): js.Promise[Boolean] = js.native
  def onDidReceiveMessage(
      listener: js.Function1[js.Any, Unit],
  ): Disposable = js.native

@js.native
trait WebviewPanel extends Disposable:
  val webview: Webview = js.native
  val visible: Boolean = js.native
  def reveal(
      viewColumn: js.UndefOr[Int] = js.undefined,
      preserveFocus: Boolean = false,
  ): Unit = js.native
  def onDidChangeViewState(
      listener: js.Function1[WebviewPanelOnDidChangeViewStateEvent, Unit],
  ): Disposable                                              = js.native
  def onDidDispose(listener: js.Function0[Unit]): Disposable = js.native

@js.native
trait WebviewPanelOnDidChangeViewStateEvent extends js.Object

@js.native
trait Uri extends js.Object:
  val fsPath: String              = js.native
  override def toString(): String = js.native

@js.native
trait UriStatic extends js.Object:
  def parse(value: String): Uri = js.native

@js.native
trait WorkspaceFolder extends js.Object:
  val uri: Uri     = js.native
  val name: String = js.native

@js.native
trait WorkspaceFoldersChangeEvent extends js.Object:
  val removed: js.Array[WorkspaceFolder] = js.native

@js.native
trait WorkspaceConfiguration extends js.Object:
  def get(section: String, defaultValue: js.Any): js.Any = js.native

@js.native
trait WorkspaceApi extends js.Object:
  val workspaceFolders: js.UndefOr[js.Array[WorkspaceFolder]] = js.native
  val isTrusted: Boolean                                      = js.native
  def getConfiguration(
      section: String,
      scope: js.UndefOr[Uri] = js.undefined,
  ): WorkspaceConfiguration = js.native
  def onDidChangeWorkspaceFolders(
      listener: js.Function1[WorkspaceFoldersChangeEvent, Unit],
  ): Disposable = js.native

@js.native
trait Window extends js.Object:
  def createOutputChannel(name: String): OutputChannel = js.native
  def createWebviewPanel(
      viewType: String,
      title: String,
      showOptions: js.Any,
      options: js.Object,
  ): WebviewPanel = js.native
  def showErrorMessage(message: String): js.Promise[js.UndefOr[String]] =
    js.native
  def showInformationMessage(message: String): js.Promise[js.UndefOr[String]] =
    js.native
  def showQuickPick(
      items: js.Array[String],
      options: js.Object,
  ): js.Promise[js.UndefOr[String]] = js.native

@js.native
trait Commands extends js.Object:
  def registerCommand(
      command: String,
      callback: js.Function0[js.Any],
  ): Disposable = js.native

@js.native
trait Env extends js.Object:
  def asExternalUri(uri: Uri): js.Promise[Uri]    = js.native
  def openExternal(uri: Uri): js.Promise[Boolean] = js.native

@js.native
@JSImport("vscode", "commands")
object VSCommands extends Commands

@js.native
@JSImport("vscode", "window")
object VSWindow extends Window

@js.native
@JSImport("vscode", "workspace")
object VSWorkspace extends WorkspaceApi

@js.native
@JSImport("vscode", "env")
object VSEnv extends Env

@js.native
@JSImport("vscode", "Uri")
object VSUri extends UriStatic
