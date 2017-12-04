package net.qwerty2501.radoc

case class Response(status: Status, headers: HeaderMap, content: Content)

object Response {
  def apply(status: Status): Response =
    new Response(status, HeaderMap(), NothingContent())
  def apply(status: Status, headers: HeaderMap): Response =
    new Response(status, headers, NothingContent())
  def apply(status: Status, text: String): Response =
    new Response(status, HeaderMap(), TextContent(text))
  def apply(status: Status, content: Content): Response =
    new Response(status, HeaderMap(), content)
  def apply(status: Status, headers: HeaderMap, text: String): Response =
    new Response(status, headers, TextContent(text))
}
