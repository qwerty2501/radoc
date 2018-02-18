package net.qwerty2501.radoc

case class Response private (status: Status,
                             headers: HeaderParameterList,
                             body: Body,
                             bodyHint: BodyHint)
    extends Message {

  private[radoc] def this(status: Status,
                          headers: Seq[Parameter],
                          body: Body,
                          bodyHint: BodyHint) =
    this(status, HeaderParameterList(headers), body, bodyHint)

  def this(status: Status, headers: Seq[Parameter], body: Body) =
    this(status, headers, body, BodyHint.empty)

  def this(status: Status) = this(status, Seq(), Body.empty, BodyHint.empty)

}

object Response {

  def apply(status: Status): Response = new Response(status)
  private[radoc] def apply(status: Status,
                           headers: Seq[Parameter],
                           body: Body,
                           bodyHint: BodyHint): Response =
    new Response(status, headers, body, bodyHint)

  private def apply(status: Status,
                    headers: HeaderParameterList,
                    body: Body,
                    bodyHint: BodyHint) =
    new Response(status, headers, body, bodyHint)

  def apply(status: Status, headers: Seq[Parameter], body: Body): Response =
    new Response(status, headers, body)

}
