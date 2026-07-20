package org.doctrellis.vscode.infrastructure.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait ReadableStream extends js.Object:
  def on(event: String, listener: js.Function): ReadableStream  = js.native
  def off(event: String, listener: js.Function): ReadableStream = js.native

@js.native
trait ChildProcess extends js.Object:
  val pid: js.UndefOr[Int]                                     = js.native
  val stdout: ReadableStream                                   = js.native
  val stderr: ReadableStream                                   = js.native
  val exitCode: js.Any                                         = js.native
  val signalCode: js.Any                                       = js.native
  def once(event: String, listener: js.Function): ChildProcess = js.native
  def off(event: String, listener: js.Function): ChildProcess  = js.native
  def kill(signal: String = "SIGTERM"): Boolean                = js.native

@js.native
trait IncomingMessage extends js.Object:
  val statusCode: js.UndefOr[Int]                       = js.native
  val headers: js.Dictionary[String | js.Array[String]] = js.native
  def resume(): Unit                                    = js.native

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
@JSImport("node:process", JSImport.Namespace)
object NodeRuntime extends js.Object:
  val env: js.Dictionary[js.UndefOr[String]]  = js.native
  val platform: String                        = js.native
  def kill(pid: Int, signal: String): Boolean = js.native
  def kill(pid: Int, signal: Int): Boolean    = js.native

@js.native
@JSImport("node:fs/promises", JSImport.Namespace)
object NodeFsPromises extends js.Object:
  def readFile(path: String, encoding: String): js.Promise[String] = js.native
  def realpath(path: String): js.Promise[String]                   = js.native
  def mkdir(path: String, options: js.Object): js.Promise[js.Any]  = js.native
  def chmod(path: String, mode: Int): js.Promise[Unit]             = js.native
  def lstat(path: String): js.Promise[NodeFileStats]               = js.native

@js.native
trait NodeFileStats extends js.Object:
  val uid: Int                  = js.native
  def isDirectory(): Boolean    = js.native
  def isSymbolicLink(): Boolean = js.native

@js.native
@JSImport("node:path", JSImport.Namespace)
object NodePath extends js.Object:
  val sep: String                                = js.native
  def join(parts: String*): String               = js.native
  def resolve(parts: String*): String            = js.native
  def relative(from: String, to: String): String = js.native
  def isAbsolute(path: String): Boolean          = js.native

@js.native
trait NodeUserInfo extends js.Object:
  val username: String = js.native
  val uid: Int         = js.native

@js.native
@JSImport("node:os", JSImport.Namespace)
object NodeOs extends js.Object:
  def tmpdir(): String         = js.native
  def userInfo(): NodeUserInfo = js.native

@js.native
trait NodeHash extends js.Object:
  def update(value: String): NodeHash  = js.native
  def digest(encoding: String): String = js.native

@js.native
trait NodeBufferConstructor extends js.Object:
  def byteLength(value: String, encoding: String): Int = js.native

@js.native
@JSImport("node:crypto", JSImport.Namespace)
object NodeCrypto extends js.Object:
  def createHash(algorithm: String): NodeHash = js.native
  def randomUUID(): String                    = js.native

@js.native
@JSImport("node:buffer", JSImport.Namespace)
object NodeBuffer extends js.Object:
  val Buffer: NodeBufferConstructor = js.native

@js.native
trait NodeSocket extends js.Object:
  def on(event: String, listener: js.Function): NodeSocket   = js.native
  def once(event: String, listener: js.Function): NodeSocket = js.native
  def off(event: String, listener: js.Function): NodeSocket  = js.native
  def write(value: String): Boolean                          = js.native
  def setEncoding(encoding: String): NodeSocket              = js.native
  def end(): Unit                                            = js.native
  def destroy(): Unit                                        = js.native

@js.native
@JSImport("node:net", JSImport.Namespace)
object NodeNet extends js.Object:
  def createConnection(options: js.Object): NodeSocket = js.native

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
