package net.qwerty2501.radoc

case class Request private (method: Method,
                            path: UrlPath,
                            headers: HeaderParameterList,
                            body: Body)
    extends Message {

  def this(method: Method,
           path: UrlPath,
           headers: Seq[Parameter] = Seq(),
           body: Body = Body()) =
    this(method, path, HeaderParameterList(headers), body)

}

object Request {

  def get(path: UrlPath, headers: Seq[Parameter] = Seq()): Request =
    apply(Method.Get, path, headers)

  def post(path: UrlPath,
           headers: Seq[Parameter] = Seq(),
           body: Body = Body()): Request =
    apply(Method.Post, path, headers, body)

  def put(path: UrlPath,
          headers: Seq[Parameter] = Seq(),
          body: Body = Body()): Request =
    apply(Method.Put, path, headers, body)

  def delete(path: UrlPath,
             headers: Seq[Parameter] = Seq(),
             body: Body = Body()): Request =
    apply(Method.Delete, path, headers, body)

  def apply(method: Method,
            path: UrlPath,
            headers: Seq[Parameter] = Seq(),
            body: Body = Body()): Request =
    new Request(method, path, headers, body)

}
