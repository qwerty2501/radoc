package net.qwerty2501.radoc

case class ApiDocumentHtmlRendererContext(
    contentHtmlRenderer: ContentHtmlRenderer) {
  def this() = this(ContentHtmlRenderer.default)
}

object ApiDocumentHtmlRendererContext {
  def apply(): ApiDocumentHtmlRendererContext =
    new ApiDocumentHtmlRendererContext()
}
