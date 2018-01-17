package net.qwerty2501.radoc

case class Request(method: Method,
                   path: Path,
                   headers: Map[String, String],
                   content: Content)
    extends Message {

  def this(method: Method, path: Path, headers: Map[String, String]) =
    this(method, path, headers, Content())

  def this(method: Method, path: String, headers: Map[String, String]) =
    this(method, Path(path), headers)

  def this(method: Method, path: Path, text: String) =
    this(method, path, Map[String, String](), Content(text))

  def this(method: Method, path: String, text: String) =
    this(method, Path(path), text)

  def this(method: Method, path: Path) =
    this(method, path, Map[String, String](), Content())

  def this(method: Method, path: String) =
    this(method, Path(path))

  def this(method: Method, path: Path, content: Content) =
    this(method, path, Map[String, String](), content)

  def this(method: Method, path: String, content: Content) =
    this(method, Path(path), content)

  def this(method: Method,
           path: Path,
           headers: Map[String, String],
           text: String) =
    this(method, path, headers, Content(text))

  def this(method: Method,
           path: String,
           headers: Map[String, String],
           text: String) =
    this(method, Path(path), headers, text)

  def this(method: Method,
           path: String,
           headers: Map[String, String],
           content: Content) =
    this(method, Path(path), headers, content)

}

object Request {

  def get(path: Path): Request = apply(Method.Get, path)
  def get(path: String): Request = apply(Method.Get, path)
  def get(path: Path, headers: Map[String, String]) =
    apply(Method.Get, path, headers)
  def get(path: String, headers: Map[String, String]) =
    apply(Method.Get, path, headers)

  def post(path: Path, content: String) = apply(Method.Post, path, content)
  def post(path: String, content: String) = apply(Method.Post, path, content)
  def post(path: Path, content: Content) = apply(Method.Post, path, content)
  def post(path: String, content: Content) = apply(Method.Post, path, content)
  def post(path: Path, headers: Map[String, String], content: String) =
    apply(Method.Post, path, headers, content)
  def post(path: String, headers: Map[String, String], content: String) =
    apply(Method.Post, path, headers, content)

  def put(path: Path, content: String) = apply(Method.Put, path, content)
  def put(path: String, content: String) = apply(Method.Put, path, content)
  def put(path: Path, content: Content) = apply(Method.Put, path, content)
  def put(path: String, content: Content) = apply(Method.Put, path, content)
  def put(path: Path, headers: Map[String, String], content: String) =
    apply(Method.Put, path, headers, content)
  def put(path: String, headers: Map[String, String], content: String) =
    apply(Method.Put, path, headers, content)

  def delete(path: Path) = apply(Method.Delete, path)
  def delete(path: String) = apply(Method.Delete, path)
  def delete(path: Path, headers: Map[String, String]) =
    apply(Method.Delete, path, headers)
  def delete(path: String, headers: Map[String, String]) =
    apply(Method.Delete, path, headers)
  def delete(path: Path, headers: Map[String, String], content: String) =
    apply(Method.Delete, path, headers, content)
  def delete(path: String, headers: Map[String, String], content: String) =
    apply(Method.Delete, path, headers, content)
  def delete(path: Path, headers: Map[String, String], content: Content) =
    apply(Method.Delete, path, headers, content)
  def delete(path: String, headers: Map[String, String], content: Content) =
    apply(Method.Delete, path, headers, content)

  def apply(method: Method, path: Path, headers: Map[String, String]) =
    new Request(method, path, headers)
  def apply(method: Method, path: String, headers: Map[String, String]) =
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
  def apply(method: Method,
            path: Path,
            headers: Map[String, String],
            text: String) =
    new Request(method, path, headers, TextContent(text))
  def apply(method: Method,
            path: String,
            headers: Map[String, String],
            text: String) =
    new Request(method, path, headers, TextContent(text))

  def apply(method: Method,
            path: String,
            headers: Map[String, String],
            content: Content) = new Request(method, path, headers, content)
}
