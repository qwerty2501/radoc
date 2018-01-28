package net.qwerty2501.radoc

case class APIDocument(method: Method,
                       path: URLPath,
                       messageDocuments: Seq[MessageDocument],
                       description: Text) {
  if (!checkRequestResponses(method, path, messageDocuments)) {
    throw new IllegalArgumentException(
      "messageDocuments should be same method and paths.")
  }

  private def checkRequestResponses(
                                     method: Method,
                                     path: URLPath,
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
