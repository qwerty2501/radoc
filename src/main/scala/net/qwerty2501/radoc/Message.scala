package net.qwerty2501.radoc

trait Message {
  val headers: HeaderMap
  val content: Content
}
