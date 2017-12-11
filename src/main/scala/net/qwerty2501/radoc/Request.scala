package net.qwerty2501.radoc

case class Request(method: Method,
                   path: Path,
                   headers: HeaderMap,
                   content: Content) {

  def this(method: Method, path: Path, headers: HeaderMap) =
    this(method, path, headers, NothingContent())

  def this(method: Method, path: String, headers: HeaderMap) =
    this(method, Path(path), headers)

  def this(method: Method, path: Path, text: String) =
    this(method, path, HeaderMap(), TextContent(text))

  def this(method: Method, path: String, text: String) =
    this(method, Path(path), text)

  def this(method: Method, path: Path) =
    this(method, path, HeaderMap(), NothingContent())

  def this(method: Method, path: String) =
    this(method, Path(path))

  def this(method: Method, path: Path, content: Content) =
    this(method, path, HeaderMap(), content)

  def this(method: Method, path: String, content: Content) =
    this(method, Path(path), content)

  def this(method: Method, path: Path, headers: HeaderMap, text: String) =
    this(method, path, headers, TextContent(text))

  def this(method: Method, path: String, headers: HeaderMap, text: String) =
    this(method, Path(path), headers, text)

  def this(method: Method, path: String, headers: HeaderMap, content: Content) =
    this(method, Path(path), headers, content)

}

object Request {

  def get(path: Path): Request = apply(Method.Get, path)
  def get(path: String): Request = apply(Method.Get, path)
  def get(path: Path, headers: HeaderMap) = apply(Method.Get, path, headers)
  def get(path: String, headers: HeaderMap) = apply(Method.Get, path, headers)

  def post(path: Path, content: String) = apply(Method.Post, path, content)
  def post(path: String, content: String) = apply(Method.Post, path, content)
  def post(path: Path, content: Content) = apply(Method.Post, path, content)
  def post(path: String, content: Content) = apply(Method.Post, path, content)
  def post(path: Path, headers: HeaderMap, content: String) =
    apply(Method.Post, path, headers, content)
  def post(path: String, headers: HeaderMap, content: String) =
    apply(Method.Post, path, headers, content)

  def put(path: Path, content: String) = apply(Method.Put, path, content)
  def put(path: String, content: String) = apply(Method.Put, path, content)
  def put(path: Path, content: Content) = apply(Method.Put, path, content)
  def put(path: String, content: Content) = apply(Method.Put, path, content)
  def put(path: Path, headers: HeaderMap, content: String) =
    apply(Method.Put, path, headers, content)
  def put(path: String, headers: HeaderMap, content: String) =
    apply(Method.Put, path, headers, content)

  def delete(path: Path) = apply(Method.Delete, path)
  def delete(path: String) = apply(Method.Delete, path)
  def delete(path: Path, headers: HeaderMap) =
    apply(Method.Delete, path, headers)
  def delete(path: String, headers: HeaderMap) =
    apply(Method.Delete, path, headers)
  def delete(path: Path, headers: HeaderMap, content: String) =
    apply(Method.Delete, path, headers, content)
  def delete(path: String, headers: HeaderMap, content: String) =
    apply(Method.Delete, path, headers, content)
  def delete(path: Path, headers: HeaderMap, content: Content) =
    apply(Method.Delete, path, headers, content)
  def delete(path: String, headers: HeaderMap, content: Content) =
    apply(Method.Delete, path, headers, content)

  def apply(method: Method, path: Path, headers: HeaderMap) =
    new Request(method, path, headers)
  def apply(method: Method, path: String, headers: HeaderMap) =
    new Request(method, path, headers)
  def apply(method: Method, path: Path, text: String) =
    new Request(method, path, TextContent(text))
  def apply(method: Method, path: String, text: String) =
    new Request(method, path, TextContent(text))
  def apply(method: Method, path: Path) =
    new Request(method, path)
  def apply(method: Method, path: String) =
    new Request(method, path)
  def apply(method: Method, path: Path, content: Content) =
    new Request(method, path, content)
  def apply(method: Method, path: String, content: Content) =
    new Request(method, path, content)
  def apply(method: Method, path: Path, headers: HeaderMap, text: String) =
    new Request(method, path, headers, TextContent(text))
  def apply(method: Method, path: String, headers: HeaderMap, text: String) =
    new Request(method, path, headers, TextContent(text))

  def apply(method: Method,
            path: String,
            headers: HeaderMap,
            content: Content) = new Request(method, path, headers, content)
}
