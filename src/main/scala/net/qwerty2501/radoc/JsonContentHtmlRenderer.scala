package net.qwerty2501.radoc

import io.circe._

import scala.xml._

object JsonContentHtmlRenderer {
  def renderer(json: Json): Node = {
    xml.Text("")
  }
}
