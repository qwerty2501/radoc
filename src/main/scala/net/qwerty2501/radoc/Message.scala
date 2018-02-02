package net.qwerty2501.radoc

trait Message {
  val headers: HeaderParameterList
  val content: Content
}
