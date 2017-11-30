package net.qwerty2501.radoc

import scala.collection.mutable

case class APIDocument private(
    private val apis: Map[APICategory, Seq[API]]) {
  def +(that: API): APIDocument =
    this + (APICategory(that.request.path) -> that)

  def +(that: (APICategory, API)): APIDocument = {
    val (category, api) = that
    val newAPIs = mutable.Map[APICategory, Seq[API]](apis.toSeq: _*)
    newAPIs.put(category, newAPIs.getOrElse(category, Nil) :+ api)
    APIDocument(newAPIs.toMap)
  }

  def test(): Unit = {
    var apiDocPart = APIDocument()
    apiDocPart += API(Request(GET, "", ""), Response(""))
  }

}

object APIDocument {
  def apply(): APIDocument = new APIDocument(Map())
}
