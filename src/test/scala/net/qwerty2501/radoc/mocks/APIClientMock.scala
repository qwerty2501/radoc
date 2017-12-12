package net.qwerty2501.radoc.mocks

import net.qwerty2501.radoc._

class APIClientMock extends APIClient {
  override def request(request: Request): Response =
    Response(Status.Ok,
             """
      | {
      |   "code":0,
      |   "message":"OK"
      | }
    """.stripMargin)
}


