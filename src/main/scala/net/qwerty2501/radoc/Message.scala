package net.qwerty2501.radoc

trait Message {
  val headers: Map[String, String]
  val content: Content
}
