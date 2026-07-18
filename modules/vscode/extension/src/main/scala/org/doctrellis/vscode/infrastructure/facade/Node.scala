package org.doctrellis.vscode.infrastructure.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait ReadableStream extends js.Object:
  def on(event: String, listener: js.Function): ReadableStream  = js.native
  def off(event: String, listener: js.Function): ReadableStream = js.native

@js.native
trait ChildProcess extends js.Object:
  val stdout: ReadableStream                                   = js.native
  val stderr: ReadableStream                                   = js.native
  val exitCode: js.Any                                         = js.native
  val signalCode: js.Any                                       = js.native
  def once(event: String, listener: js.Function): ChildProcess = js.native
  def off(event: String, listener: js.Function): ChildProcess  = js.native
  def kill(signal: String = "SIGTERM"): Boolean                = js.native

@js.native
trait IncomingMessage extends js.Object:
  val statusCode: js.UndefOr[Int] = js.native
  def resume(): Unit              = js.native

@js.native
trait ClientRequest extends js.Object:
  def once(event: String, listener: js.Function): ClientRequest = js.native
  def off(event: String, listener: js.Function): ClientRequest  = js.native
  def destroy(): Unit                                           = js.native

@js.native
@JSImport("node:child_process", JSImport.Namespace)
object NodeChildProcess extends js.Object:
  def spawn(
      command: String,
      args: js.Array[String],
      options: js.Object,
  ): ChildProcess = js.native

@js.native
@JSImport("node:fs/promises", JSImport.Namespace)
object NodeFsPromises extends js.Object:
  def readFile(path: String, encoding: String): js.Promise[String] = js.native
  def realpath(path: String): js.Promise[String]                   = js.native

@js.native
@JSImport("node:path", JSImport.Namespace)
object NodePath extends js.Object:
  val sep: String                                = js.native
  def join(parts: String*): String               = js.native
  def resolve(parts: String*): String            = js.native
  def relative(from: String, to: String): String = js.native
  def isAbsolute(path: String): Boolean          = js.native

@js.native
@JSImport("node:http", JSImport.Namespace)
object NodeHttp extends js.Object:
  def get(
      url: String,
      listener: js.Function1[IncomingMessage, Unit],
  ): ClientRequest = js.native

@js.native
@JSImport("node:https", JSImport.Namespace)
object NodeHttps extends js.Object:
  def get(
      url: String,
      listener: js.Function1[IncomingMessage, Unit],
  ): ClientRequest = js.native
