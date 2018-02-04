package net.qwerty2501.radoc

case class ApiDocument(method: Method,
                       path: UrlPath,
                       messageDocumentMap: Map[String, MessageDocument],
                       description: Text,
                       group: String,
                       category: String,
                       version: Version) {

  override def toString: String =
    method.toString + " " + path.displayPath.toString
  if (!checkRequestResponses(method, path, messageDocumentMap.values.toSeq)) {
    throw new IllegalArgumentException(
      "messageDocuments should be same method and paths.")
  }

  private def checkRequestResponses(
                                     method: Method,
                                     path: UrlPath,
                                     requestResponses: Seq[MessageDocument]): Boolean = {
    if (requestResponses.isEmpty) { return true }
    val head = requestResponses.head
    requestResponses
      .count(requestResponse =>
        requestResponse.request.method == head.request.method && requestResponse.request.method == method &&
          requestResponse.request.path.displayPath == head.request.path.displayPath &&
          requestResponse.request.path.displayPath == path.displayPath) == requestResponses.length
  }
}
