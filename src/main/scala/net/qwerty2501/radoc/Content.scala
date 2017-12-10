package net.qwerty2501.radoc

trait Content {}

object Content {
  def apply(): Content = NothingContent()
  def apply(text: String): Content =
    if (text != "") TextContent(text) else NothingContent()
}

private case class NothingContent() extends Content {
  override def toString: String = ""
}

private case class TextContent(text: String) extends Content {
  override def toString = text
}
