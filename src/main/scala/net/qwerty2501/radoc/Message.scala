package net.qwerty2501.radoc

case class Message(headers: Map[String, String], content: Content)

object Message {

  def apply(headers: Map[String, String]) = new Message(headers, Content())

  def apply(content: Content) = new Message(Map(), content)

  def apply() = new Message(Map(), Content())
}
