package net.qwerty2501.radoc

case class Request(method: Method,
                   path: String,
                   headers: HeaderMap,
                   content: Content) {}

object Request {

  def apply(method: Method, path: String, text: String): Request =
    new Request(method, path, HeaderMap(), TextContent(text))
  def apply(method: Method, path: String): Request =
    new Request(method, path, HeaderMap(), NothingContent())
  def apply(method: Method, path: String, content: Content): Request =
    new Request(method, path, HeaderMap(), content)
  def apply(method: Method,
            path: String,
            headers: HeaderMap,
            text: String): Request =
    new Request(method, path, headers, TextContent(text))
}
