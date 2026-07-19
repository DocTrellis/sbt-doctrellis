package org.doctrellis.vscode.infrastructure

import org.doctrellis.vscode.domain.Workspace

private[infrastructure] object WorkspaceOptions:
  def from(workspaces: Vector[Workspace]): Vector[(String, Workspace)] =
    workspaces.map(workspace =>
      s"${workspace.name} — ${workspace.root}" -> workspace,
    )

private[infrastructure] object WorkspaceTrust:
  def validate(
      required: Boolean,
      trusted: Boolean,
  ): Either[IllegalStateException, Unit] =
    Either.cond(
      !required || trusted,
      (),
      IllegalStateException(
        "DocTrellis cannot run preview commands in an untrusted workspace.",
      ),
    )
