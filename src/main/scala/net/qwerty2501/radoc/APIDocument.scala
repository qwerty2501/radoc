package net.qwerty2501.radoc

private case class APIDocument(messageDocuments: Seq[MessageDocument],
                               description: String) {
  if (checkRequestResponses(messageDocuments) == false) {
    throw new IllegalArgumentException(
      "messageDocuments should be same method and paths.")
  }

  private def checkRequestResponses(
      requestResponses: Seq[MessageDocument]): Boolean = {

    val head = requestResponses.head
    requestResponses
      .filter(requestResponse =>
        requestResponse.request.method == head.request.method && requestResponse.request.path.displayPath == head.request.path.displayPath)
      .length == requestResponses.length
  }
}
