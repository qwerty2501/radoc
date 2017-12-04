package net.qwerty2501.radoc

import scala.collection.mutable

case class APIDocument(apis: Map[String, RequestResponseContainer]) {}

object APIDocument {

  def apply(requestResponses: Seq[(Request, Response)]): APIDocument =
    createFromRequestResponseDocuments(requestResponses.map { that =>
      RequestResponseDocument(that._1, that._2)
    })

  def createFromRequestResponseDocuments(
      requestResponseDocuments: Seq[RequestResponseDocument]): APIDocument = {
    APIDocument(
      requestResponseDocuments.groupBy(_.requestResponses.head._1.path).map {
        that =>
          that._1 -> RequestResponseContainer(that._2)
      })
  }
}
