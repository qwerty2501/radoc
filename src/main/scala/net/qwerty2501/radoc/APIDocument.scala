package net.qwerty2501.radoc

case class APIDocument private (
    var documentParts: Map[Version, APIDocumentPart])

object APIDocument {
  def apply(): APIDocument = new APIDocument(Map())
}
