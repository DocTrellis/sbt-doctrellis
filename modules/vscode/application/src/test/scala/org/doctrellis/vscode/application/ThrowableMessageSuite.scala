package org.doctrellis.vscode.application

class ThrowableMessageSuite extends munit.FunSuite:
  test("normalizes throwable messages"):
    assertEquals(ThrowableMessage.from(new Throwable("failure")), "failure")
    List(new Throwable(), new Throwable("")).foreach: problem =>
      assertEquals(ThrowableMessage.from(problem), problem.toString)
