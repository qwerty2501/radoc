package net.qwerty2501.radoc

case class APIDocument(messageDocuments: Seq[MessageDocument],
                       description: String,
                       extendArguments: Map[String, String]) {
  if (!checkRequestResponses(messageDocuments)) {
    throw new IllegalArgumentException(
      "messageDocuments should be same method and paths.")
  }

  private def checkRequestResponses(
      requestResponses: Seq[MessageDocument]): Boolean = {
    if (requestResponses.isEmpty) { return true }
    val head = requestResponses.head
    requestResponses
      .count(requestResponse =>
        requestResponse.request.method == head.request.method && requestResponse.request.path.displayPath == head.request.path.displayPath) == requestResponses.length
  }
}
