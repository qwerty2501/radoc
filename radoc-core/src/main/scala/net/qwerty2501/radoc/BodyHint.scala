package net.qwerty2501.radoc

trait BodyHint {

  val rootTypeName: String
  val typeParameterMap: Map[String, Seq[Parameter]]
}

object BodyHint {
  val empty: BodyHint = new BodyHint {
    override val rootTypeName: String = ""
    override val typeParameterMap: Map[String, Seq[Parameter]] = Map()
  }
  def apply(): BodyHint = empty
}
