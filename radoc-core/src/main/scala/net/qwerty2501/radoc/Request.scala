package net.qwerty2501.radoc

case class Request private (method: Method,
                            path: UrlPath,
                            headers: HeaderParameterList,
                            body: Body)
    extends Message {

  def this(method: Method, path: UrlPath, headers: Seq[Parameter], body: Body) =
    this(method, path, HeaderParameterList(headers), body)

  def this(method: Method, path: UrlPath, headers: Seq[Parameter]) =
    this(method, path, headers, Body())

  def this(method: Method, path: UrlPath, text: String) =
    this(method, path, Seq(), Body(text))

  def this(method: Method, path: UrlPath) =
    this(method, path, Seq(), Body())

  def this(method: Method, path: UrlPath, body: Body) =
    this(method, path, Seq(), body)

  def this(method: Method,
           path: UrlPath,
           headers: Seq[Parameter],
           text: String) =
    this(method, path, headers, Body(text))

}

object Request {

  def get(path: UrlPath): Request = apply(Method.Get, path)

  def get(path: UrlPath, headers: Seq[Parameter]): Request =
    apply(Method.Get, path, headers)

  def post(path: UrlPath, body: String): Request =
    apply(Method.Post, path, body)

  def post(path: UrlPath, body: Body): Request =
    apply(Method.Post, path, body)

  def post(path: UrlPath, headers: Seq[Parameter], body: String): Request =
    apply(Method.Post, path, headers, body)

  def put(path: UrlPath, body: String): Request =
    apply(Method.Put, path, body)

  def put(path: UrlPath, body: Body): Request =
    apply(Method.Put, path, body)

  def put(path: UrlPath, headers: Seq[Parameter], body: String): Request =
    apply(Method.Put, path, headers, body)

  def delete(path: UrlPath): Request = apply(Method.Delete, path)

  def delete(path: UrlPath, headers: Seq[Parameter]): Request =
    apply(Method.Delete, path, headers)

  def delete(path: UrlPath, headers: Seq[Parameter], body: String): Request =
    apply(Method.Delete, path, headers, body)

  def delete(path: UrlPath, headers: Seq[Parameter], body: Body): Request =
    apply(Method.Delete, path, headers, body)

  def apply(method: Method,
            path: UrlPath,
            headers: Seq[Parameter],
            body: Body): Request =
    new Request(method, path, headers, body)

  def apply(method: Method, path: UrlPath, headers: Seq[Parameter]): Request =
    new Request(method, path, headers)

  def apply(method: Method, path: UrlPath, text: String): Request =
    new Request(method, path, Body(text))

  def apply(method: Method, path: UrlPath): Request =
    new Request(method, path)

  def apply(method: Method, path: UrlPath, body: Body): Request =
    new Request(method, path, body)

  def apply(method: Method,
            path: UrlPath,
            headers: Seq[Parameter],
            text: String): Request =
    new Request(method, path, headers, Body(text))

  private def apply(method: Method,
                    path: UrlPath,
                    headers: HeaderParameterList,
                    body: Body) =
    new Request(method, path, headers, body)

}
