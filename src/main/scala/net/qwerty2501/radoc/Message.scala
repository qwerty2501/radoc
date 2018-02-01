package net.qwerty2501.radoc

trait Message {
  val headerMap: HeaderMap
  val content: Content
}
