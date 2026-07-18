package org.doctrellis.vscode.infrastructure

import cats.effect.std.Dispatcher
import cats.effect.{IO, Resource}
import org.doctrellis.vscode.application.{PreviewPorts, PreviewService}
import org.doctrellis.vscode.infrastructure.facade.ExtensionContext

private[vscode] object ExtensionRuntime:
  def resource(context: ExtensionContext): Resource[IO, Unit] =
    for
      output     <- VscodeOutput.resource
      dispatcher <- Dispatcher.parallel[IO](await = false)
      ports = PreviewPorts(
        NodeConfig.load,
        VscodePreferences.load,
        NodeProcess.launch(dispatcher, output.append),
        NodeHttpProbe.ready,
        VscodePreview.open,
        output.info,
        output.report,
      )
      service <- PreviewService.resource(ports)
      _       <- CommandBindings.resource(context, dispatcher, service, output)
      _       <- Resource.eval(
        output.append(
          "DocTrellis extension activated. Preview starts only on command.\n",
        ),
      )
    yield ()
