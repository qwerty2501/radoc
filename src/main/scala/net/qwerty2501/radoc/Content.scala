package net.qwerty2501.radoc

trait Content {}

case class NothingContent() extends Content {
  override def toString: String = ""
}

case class TextContent(text: String) extends Content {
  override def toString = text
}
