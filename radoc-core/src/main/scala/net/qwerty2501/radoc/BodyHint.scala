package net.qwerty2501.radoc

trait BodyHint {

  val rootTypeName: String
  val typeParameterMap: Map[String, Seq[Parameter]]
}
