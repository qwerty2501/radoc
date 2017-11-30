package net.qwerty2501.radoc

case class APIDocumentPart private (var apis: Map[APICategory, Seq[API]])

object APIDocumentPart {
  def apply(): APIDocumentPart = new APIDocumentPart(Map())
}
