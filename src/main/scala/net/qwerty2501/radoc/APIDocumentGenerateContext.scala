package net.qwerty2501.radoc

case class APIDocumentGenerateContext(
    rootAPIDocumentTemplatePath: String,
    rootAPIDocumentWithVersionTemplatePath: String) {
  def this() =
    this("net.qwerty2501.radoc/rootAPIDocument.ssp",
         "rootAPIDocumentWithVersion.ssp")
}

object APIDocumentGenerateContext {
  def apply() = new APIDocumentGenerateContext()
}
