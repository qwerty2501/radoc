package net.qwerty2501.radoc

final class Essentiality private () {}

object Essentiality {
  val optional = new Essentiality()
  val mandatory = new Essentiality()
}
