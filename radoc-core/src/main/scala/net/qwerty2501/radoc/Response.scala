package net.qwerty2501.radoc

case class Response(status: Status,
                    headers: HeaderParameterList,
                    body: Body)
    extends Message {

  def this(status: Status, headers: Seq[Parameter], content: Body) =
    this(status, HeaderParameterList(headers), content)
  def this(status: Status) =
    this(status, Seq(), Body())

  def this(status: Status, headers: Seq[Parameter]) =
    this(status, headers, Body())

  def this(status: Status, text: String) =
    this(status, Seq(), Body(text))

  def this(status: Status, content: Body) =
    this(status, Seq(), content)
  def this(status: Status, headers: Seq[Parameter], text: String) =
    this(status, headers, Body(text))

}

object Response {
  def apply(status: Status): Response =
    new Response(status)
  def apply(status: Status, headers: Seq[Parameter]): Response =
    new Response(status, headers)
  def apply(status: Status, text: String): Response =
    new Response(status, text)
  def apply(status: Status, content: Body): Response =
    new Response(status, content)
  def apply(status: Status, headers: Seq[Parameter], text: String): Response =
    new Response(status, headers, text)

  def apply(status: Status,
            headers: Seq[Parameter],
            content: Body): Response = new Response(status, headers, content)

  private def apply(status: Status,
                    headers: HeaderParameterList,
                    content: Body): Response =
    new Response(status, headers, content)

}
