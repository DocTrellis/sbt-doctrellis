package org.doctrellis.vscode.infrastructure.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("node:process", JSImport.Namespace)
private[infrastructure] object TestNodeRuntime extends js.Object:
  val execPath: String = js.native
  def cwd(): String    = js.native

@js.native
@JSImport("node:fs/promises", JSImport.Namespace)
private[infrastructure] object TestNodeFsPromises extends js.Object:
  def mkdtemp(prefix: String): js.Promise[String] = js.native
  def cp(
      source: String,
      destination: String,
      options: js.Object,
  ): js.Promise[Unit] =
    js.native
  def mkdir(path: String, options: js.Object): js.Promise[js.Any] = js.native
  def access(path: String): js.Promise[Unit]                      = js.native
  def writeFile(path: String, data: String): js.Promise[Unit]     = js.native
  def symlink(target: String, path: String): js.Promise[Unit]     = js.native
  def rm(path: String, options: js.Object): js.Promise[Unit]      = js.native
