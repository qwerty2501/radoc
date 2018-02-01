package net.qwerty2501.radoc

case class Request(method: Method,
                   path: URLPath,
                   headerMap: HeaderMap,
                   content: Content)
    extends Message {

  def this(method: Method, path: URLPath, headers: HeaderMap) =
    this(method, path, headers, Content())

  def this(method: Method, path: URLPath, text: String) =
    this(method, path, Map[String, HeaderParameter](), Content(text))

  def this(method: Method, path: URLPath) =
    this(method, path, Map[String, HeaderParameter](), Content())

  def this(method: Method, path: URLPath, content: Content) =
    this(method, path, Map[String, HeaderParameter](), content)

  def this(method: Method, path: URLPath, headers: HeaderMap, text: String) =
    this(method, path, headers, Content(text))

}

object Request {

  def get(path: URLPath): Request = apply(Method.Get, path)

  def get(path: URLPath, headers: HeaderMap) =
    apply(Method.Get, path, headers)

  def post(path: URLPath, content: String) = apply(Method.Post, path, content)

  def post(path: URLPath, content: Content) = apply(Method.Post, path, content)

  def post(path: URLPath, headers: HeaderMap, content: String) =
    apply(Method.Post, path, headers, content)

  def put(path: URLPath, content: String) = apply(Method.Put, path, content)

  def put(path: URLPath, content: Content) = apply(Method.Put, path, content)

  def put(path: URLPath, headers: HeaderMap, content: String) =
    apply(Method.Put, path, headers, content)

  def delete(path: URLPath) = apply(Method.Delete, path)

  def delete(path: URLPath, headers: HeaderMap) =
    apply(Method.Delete, path, headers)

  def delete(path: URLPath, headers: HeaderMap, content: String) =
    apply(Method.Delete, path, headers, content)

  def delete(path: URLPath, headers: HeaderMap, content: Content) =
    apply(Method.Delete, path, headers, content)

  def apply(method: Method, path: URLPath, headers: HeaderMap) =
    new Request(method, path, headers)

  def apply(method: Method, path: URLPath, text: String) =
    new Request(method, path, TextContent(text))

  def apply(method: Method, path: URLPath) =
    new Request(method, path)

  def apply(method: Method, path: URLPath, content: Content) =
    new Request(method, path, content)

  def apply(method: Method, path: URLPath, headers: HeaderMap, text: String) =
    new Request(method, path, headers, TextContent(text))

}
