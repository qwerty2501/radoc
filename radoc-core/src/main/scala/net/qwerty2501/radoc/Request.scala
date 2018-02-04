package net.qwerty2501.radoc

case class Request private (method: Method,
                            path: UrlPath,
                            headers: HeaderParameterList,
                            content: Content)
    extends Message {

  def this(method: Method,
           path: UrlPath,
           headers: Seq[Parameter],
           content: Content) =
    this(method, path, HeaderParameterList(headers), content)

  def this(method: Method, path: UrlPath, headers: Seq[Parameter]) =
    this(method, path, headers, Content())

  def this(method: Method, path: UrlPath, text: String) =
    this(method, path, Seq(), Content(text))

  def this(method: Method, path: UrlPath) =
    this(method, path, Seq(), Content())

  def this(method: Method, path: UrlPath, content: Content) =
    this(method, path, Seq(), content)

  def this(method: Method,
           path: UrlPath,
           headers: Seq[Parameter],
           text: String) =
    this(method, path, headers, Content(text))

}

object Request {

  def get(path: UrlPath): Request = apply(Method.Get, path)

  def get(path: UrlPath, headers: Seq[Parameter]): Request =
    apply(Method.Get, path, headers)

  def post(path: UrlPath, content: String): Request =
    apply(Method.Post, path, content)

  def post(path: UrlPath, content: Content): Request =
    apply(Method.Post, path, content)

  def post(path: UrlPath, headers: Seq[Parameter], content: String): Request =
    apply(Method.Post, path, headers, content)

  def put(path: UrlPath, content: String): Request =
    apply(Method.Put, path, content)

  def put(path: UrlPath, content: Content): Request =
    apply(Method.Put, path, content)

  def put(path: UrlPath, headers: Seq[Parameter], content: String): Request =
    apply(Method.Put, path, headers, content)

  def delete(path: UrlPath): Request = apply(Method.Delete, path)

  def delete(path: UrlPath, headers: Seq[Parameter]): Request =
    apply(Method.Delete, path, headers)

  def delete(path: UrlPath, headers: Seq[Parameter], content: String): Request =
    apply(Method.Delete, path, headers, content)

  def delete(path: UrlPath,
             headers: Seq[Parameter],
             content: Content): Request =
    apply(Method.Delete, path, headers, content)

  def apply(method: Method,
            path: UrlPath,
            headers: Seq[Parameter],
            content: Content): Request =
    new Request(method, path, headers, content)

  def apply(method: Method, path: UrlPath, headers: Seq[Parameter]): Request =
    new Request(method, path, headers)

  def apply(method: Method, path: UrlPath, text: String): Request =
    new Request(method, path, Content(text))

  def apply(method: Method, path: UrlPath): Request =
    new Request(method, path)

  def apply(method: Method, path: UrlPath, content: Content): Request =
    new Request(method, path, content)

  def apply(method: Method,
            path: UrlPath,
            headers: Seq[Parameter],
            text: String): Request =
    new Request(method, path, headers, Content(text))

  private def apply(method: Method,
                    path: UrlPath,
                    headers: HeaderParameterList,
                    content: Content) =
    new Request(method, path, headers, content)

}
