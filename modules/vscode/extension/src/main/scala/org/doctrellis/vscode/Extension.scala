package org.doctrellis.vscode

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.doctrellis.vscode.infrastructure.ExtensionRuntime
import org.doctrellis.vscode.infrastructure.facade.ExtensionContext

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object Extension:
  private given IORuntime       = IORuntime.global
  private var release: IO[Unit] = IO.unit

  @JSExportTopLevel(name = "activate", moduleID = "extension")
  def activate(context: ExtensionContext): js.Promise[Unit] =
    ExtensionRuntime
      .resource(context)
      .allocated
      .flatMap:
        case (_, finalizer) =>
          IO.delay:
            release = finalizer
      .unsafeToPromise()

  @JSExportTopLevel(name = "deactivate", moduleID = "extension")
  def deactivate(): js.Promise[Unit] =
    releaseCurrent.unsafeToPromise()

  private def releaseCurrent: IO[Unit] =
    IO.defer:
      val current = release
      release = IO.unit
      current
