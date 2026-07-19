package org.doctrellis.vscode.infrastructure

import cats.effect.{IO, Resource}
import cats.effect.std.Dispatcher

import org.doctrellis.vscode.application.{PreviewPorts, PreviewService}
import org.doctrellis.vscode.infrastructure.facade.ExtensionContext

private[vscode] object ExtensionRuntime:
  def resource(context: ExtensionContext): Resource[IO, Unit] =
    for
      output     <- VscodeOutput.resource
      dispatcher <- Dispatcher.parallel[IO](await = false)
      panels     <- PreviewPanelRegistry.resource(
        VscodePreviewPanelBackend(dispatcher),
      )
      ports = PreviewPorts[IO](
        load = NodeConfig.load,
        policy = VscodePreferences.load,
        launch = WorkspacePreview.launch(dispatcher, output.append),
        probe = NodeHttpProbe.ready,
        show = panels.show,
        update = panels.update,
        openExternal = VscodeExternalPreview.open,
        info = output.info,
        error = output.report,
      )
      service <- PreviewService.resource(ports)
      _       <- CommandBindings.resource(context, dispatcher, service, output)
      _       <- Resource.eval(
        output.append(
          "DocTrellis extension activated. Preview starts only on command.\n",
        ),
      )
    yield ()
