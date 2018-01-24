package net.qwerty2501.radoc

import scala.collection.immutable

trait HeaderMap extends Map[String, String]

object HeaderMap {
  def apply(): HeaderMap = new HeaderMapImpl()
}

private class HeaderMapImpl
    extends immutable.HashMap[String, String]
    with HeaderMap {}
