package net.qwerty2501.radoc

case class Request private (method: Method,
                            path: UrlPath,
                            headers: HeaderParameterList,
                            body: Body,
                            bodyHint: BodyHint)
    extends Message {

  private[radoc] def this(method: Method,
                          path: UrlPath,
                          headers: Seq[Parameter],
                          body: Body,
                          bodyHint: BodyHint) =
    this(method, path, HeaderParameterList(headers), body, bodyHint)
  def this(method: Method, path: UrlPath, headers: Seq[Parameter], body: Body) =
    this(method, path, HeaderParameterList(headers), body, BodyHint())

}

object Request {

  def get(path: UrlPath): Request = get(path, Seq())
  def get(path: UrlPath, headers: Seq[Parameter]): Request =
    apply(Method.Get, path, headers, Body())

  def post(path: UrlPath, body: Body): Request = post(path, Seq(), body)
  def post(path: UrlPath, headers: Seq[Parameter], body: Body): Request =
    apply(Method.Post, path, headers, body)

  def put(path: UrlPath, body: Body): Request = put(path, Seq(), body)
  def put(path: UrlPath, headers: Seq[Parameter], body: Body): Request =
    apply(Method.Put, path, headers, body)

  def delete(path: UrlPath): Request = delete(path, Seq(), Body())
  def delete(path: UrlPath, headers: Seq[Parameter], body: Body): Request =
    apply(Method.Delete, path, headers, body)

  private def apply(method: Method,
                    path: UrlPath,
                    headers: HeaderParameterList,
                    body: Body,
                    bodyHint: BodyHint) =
    new Request(method, path, headers, body, bodyHint)
  private[radoc] def apply(method: Method,
                           path: UrlPath,
                           headers: Seq[Parameter],
                           body: Body,
                           bodyHint: BodyHint): Request =
    new Request(method, path, headers, body, bodyHint)

  def apply(method: Method,
            path: UrlPath,
            headers: Seq[Parameter],
            body: Body): Request =
    new Request(method, path, headers, body)

}
