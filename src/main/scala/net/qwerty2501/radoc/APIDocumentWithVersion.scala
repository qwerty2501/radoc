package net.qwerty2501.radoc

case class APIDocumentWithVersion private(
    private var documentParts: Map[Version, APIDocument])

object APIDocumentWithVersion {
  def apply(): APIDocumentWithVersion = new APIDocumentWithVersion(Map())
}
