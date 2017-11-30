package net.qwerty2501.radoc

case class APIIdentifier private (identifier: String)

object APIIdentifier {
  def apply(): APIIdentifier = new APIIdentifier("")
}
