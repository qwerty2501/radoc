package net.qwerty2501.radoc

case class APIDocument(apis: Map[String, RequestResponseContainer]) {}

object APIDocument {

  def createFromRequestResponsePairs(
      requestResponses: Seq[(Request, Response)]): APIDocument =
    new APIDocument(
      APIDocument.createAPIsMap(
        requestResponses
          .groupBy(that => that._1.method -> that._1.path)
          .map(that => RequestResponseDocument(that._2))
          .toSeq))

  private def createAPIsMap(
      requestResponseDocuments: Seq[RequestResponseDocument]) =
    requestResponseDocuments.groupBy(_.requestResponses.head._1.path).map {
      that =>
        that._1.toString -> RequestResponseContainer(that._2)
    }
  def createFromRequestResponseDocuments(
      requestResponseDocuments: Seq[RequestResponseDocument]): APIDocument =
    APIDocument(createAPIsMap(requestResponseDocuments))
}
