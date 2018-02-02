package net.qwerty2501.radoc

case class Response(status: Status,
                    headers: HeaderParameterList,
                    content: Content)
    extends Message {

  def this(status: Status, headers: Seq[Parameter], content: Content) =
    this(status, HeaderParameterList(headers), content)
  def this(status: Status) =
    this(status, Seq(), Content())

  def this(status: Status, headers: Seq[Parameter]) =
    this(status, headers, Content())

  def this(status: Status, text: String) =
    this(status, Seq(), Content(text))

  def this(status: Status, content: Content) =
    this(status, Seq(), content)
  def this(status: Status, headers: Seq[Parameter], text: String) =
    this(status, headers, Content(text))

}

object Response {
  def apply(status: Status): Response =
    new Response(status)
  def apply(status: Status, headers: Seq[Parameter]): Response =
    new Response(status, headers)
  def apply(status: Status, text: String): Response =
    new Response(status, text)
  def apply(status: Status, content: Content): Response =
    new Response(status, content)
  def apply(status: Status, headers: Seq[Parameter], text: String): Response =
    new Response(status, headers, text)

  def apply(status: Status,
            headers: Seq[Parameter],
            content: Content): Response = new Response(status, headers, content)

  private def apply(status: Status,
                    headers: HeaderParameterList,
                    content: Content): Response =
    new Response(status, headers, content)

}
