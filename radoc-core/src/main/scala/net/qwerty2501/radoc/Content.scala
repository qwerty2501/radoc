package net.qwerty2501.radoc

import scala.xml._
import io.circe._, io.circe.parser._
trait Content {
  def contentText: String
}

object Content {
  def apply(): Content = TextContent("")
  def apply(text: String): Content = TextContent(text)

}

private case class TextContent(text: String) extends Content {
  override def contentText: String = text

}
