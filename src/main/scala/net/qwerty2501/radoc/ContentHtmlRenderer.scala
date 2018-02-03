package net.qwerty2501.radoc

import io.circe.parser.parse

import scala.xml._

trait ContentHtmlRenderer {
  def render(message: Message, renderArguments: HtmlRenderArguments): Node
}

object ContentHtmlRenderer {
  val default: ContentHtmlRenderer = new DefaultContentHtmlRenderer()
}

private class DefaultContentHtmlRenderer extends ContentHtmlRenderer {
  def render(message: Message, renderArguments: HtmlRenderArguments): Node = {

    <pre style="color:white;"><code>{ContentType(message.headers) match {
      case ContentType.Xml  => renderXml(message, renderArguments)
      case ContentType.Json => renderJson(message, renderArguments)
      case _                => renderText(message, renderArguments)
    }}</code></pre>

  }

  private def renderText(message: Message,
                         renderArguments: HtmlRenderArguments): Node = {
    Unparsed(message.content.contentText)
  }

  private def renderJson(message: Message,
                         renderArguments: HtmlRenderArguments): Node = {
    parse(message.content.contentText) match {
      case Left(e)     => renderText(message, renderArguments)
      case Right(json) => JsonContentHtmlRenderer.renderer(json)
    }
  }

  private def renderXml(message: Message,
                        renderArguments: HtmlRenderArguments): Node = {
    Unparsed(Xhtml.toXhtml(XML.loadString(message.content.contentText)))
  }
}
