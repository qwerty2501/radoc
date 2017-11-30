package net.qwerty2501.radoc

case class Response private (headers: HeaderMap, content: Content)

object Response {
  def apply(): Response = new Response(HeaderMap(), NothingContent())
  def apply(headers: HeaderMap): Response =
    new Response(headers, NothingContent())
  def apply(text: String): Response =
    new Response(HeaderMap(), TextContent(text))
  def apply(content: Content): Response = new Response(HeaderMap(), content)
  def apply(headers: HeaderMap, text: String): Response =
    new Response(headers, TextContent(text))
}
