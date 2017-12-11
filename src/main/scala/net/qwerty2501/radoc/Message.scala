package net.qwerty2501.radoc

case class Message(headers: HeaderMap, content: Content)

object Message {

  def apply(headers: HeaderMap) = new Message(headers, Content())

  def apply(content: Content) = new Message(HeaderMap(), content)

  def apply() = new Message(HeaderMap(), Content())
}
