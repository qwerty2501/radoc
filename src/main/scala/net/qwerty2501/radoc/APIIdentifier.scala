package net.qwerty2501.radoc

case class APIIdentifier(identifier: String)

object APIIdentifier {
  def apply(): APIIdentifier = new APIIdentifier("")
}
