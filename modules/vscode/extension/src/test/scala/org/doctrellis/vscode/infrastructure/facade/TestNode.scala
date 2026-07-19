package org.doctrellis.vscode.infrastructure.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("node:process", JSImport.Namespace)
private[infrastructure] object TestNodeRuntime extends js.Object:
  val execPath: String = js.native
  def cwd(): String    = js.native
