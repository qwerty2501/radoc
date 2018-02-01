package net.qwerty2501.radoc

import scala.xml._
import io.circe._, io.circe.parser._
trait Content {
  private[radoc] def renderHtml(contentType: ContentType): Node
}

object Content {
  def apply(): Content = NothingContent()
  def apply(text: String): Content =
    if (text != "") TextContent(text) else NothingContent()

}

private case class NothingContent() extends Content {

  override private[radoc] def renderHtml(contentType: ContentType) =
    xml.Text("")
}

private case class TextContent(text: String) extends Content {
  override private[radoc] def renderHtml(contentType: ContentType) =
    contentType match {
      case ContentType.Xml  => renderXml
      case ContentType.Json => renderJson
      case _                => xml.Text(text)
    }

  private def renderJson: Node = {
    parse(text) match {
      case Left(e)     => xml.Text(text)
      case Right(json) => xml.Text(json.toString)
    }
  }

  private def renderXml: Node = {
    Unparsed(Xhtml.toXhtml(XML.loadString(text)))
  }
}
