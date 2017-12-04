package net.qwerty2501.radoc

case class RequestResponseDocument(id: String,
                                   requestResponses: Seq[(Request, Response)],
                                   extendArgs: Map[String, String]) {

  if (checkRequestResponses(requestResponses) == false) {
    throw new IllegalArgumentException(
      "requestResponses should be same method and paths.")
  }

  def this(request: Request, response: Response) =
    this(request, response, Map[String, String]())

  def this(request: Request,
           response: Response,
           extendArgs: Map[String, String]) =
    this("", Seq((request, response)), extendArgs)

  def this(requestResponses: Seq[(Request, Response)],
           extendArgs: Map[String, String]) =
    this("", requestResponses, extendArgs)

  def this(requestResponses: Seq[(Request, Response)]) =
    this(requestResponses, Map[String, String]())

  private def checkRequestResponses(
      requestResponses: Seq[(Request, Response)]): Boolean = {

    val head = requestResponses.head
    requestResponses
      .filter(requestResponse =>
        requestResponse._1.method == head._1.method && requestResponse._1.path == head._1.path)
      .length == requestResponses.length
  }
}

object RequestResponseDocument {

  def apply(request: Request, response: Response): RequestResponseDocument =
    new RequestResponseDocument(request, response)

  def apply(extendArgs: Map[String, String],
            request: Request,
            response: Response): RequestResponseDocument =
    new RequestResponseDocument(request, response, extendArgs)

  def apply(requestResponses: Seq[(Request, Response)],
            extendArgs: Map[String, String]) =
    new RequestResponseDocument(requestResponses, extendArgs)

  def apply(requestResponses: Seq[(Request, Response)]) =
    new RequestResponseDocument(requestResponses)

}
