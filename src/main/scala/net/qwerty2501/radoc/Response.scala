package net.qwerty2501.radoc

case class Response(status: Status, headerMap: HeaderMap, content: Content)
    extends Message {
  def this(status: Status) =
    this(status, Map[String, HeaderParameter](), Content())

  def this(status: Status, headers: HeaderMap) =
    this(status, headers, Content())

  def this(status: Status, text: String) =
    this(status, Map[String, HeaderParameter](), Content(text))

  def this(status: Status, content: Content) =
    this(status, Map[String, HeaderParameter](), content)
  def this(status: Status, headers: HeaderMap, text: String) =
    this(status, headers, Content(text))

}

object Response {
  def apply(status: Status): Response =
    new Response(status)
  def apply(status: Status, headers: HeaderMap): Response =
    new Response(status, headers)
  def apply(status: Status, text: String): Response =
    new Response(status, text)
  def apply(status: Status, content: Content): Response =
    new Response(status, content)
  def apply(status: Status, headers: HeaderMap, text: String): Response =
    new Response(status, headers, text)
}
