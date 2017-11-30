package net.qwerty2501.radoc

sealed trait Method {
  override def toString = this.getClass().getName()
}

case object GET extends Method
case object POST extends Method
case object PUT extends Method
case object DELETE extends Method
