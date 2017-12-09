package net.qwerty2501.radoc

case class Status(statusCode: Int) {

  override def toString = "Status:" + statusCode

}

object Statuses {
  final val OK = Status(200)
}
