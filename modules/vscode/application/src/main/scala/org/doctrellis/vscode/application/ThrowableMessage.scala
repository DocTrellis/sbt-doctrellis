package org.doctrellis.vscode.application

private[vscode] object ThrowableMessage:
  def from(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString)
