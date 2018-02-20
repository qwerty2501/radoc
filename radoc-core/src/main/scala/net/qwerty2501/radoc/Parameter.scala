package net.qwerty2501.radoc

trait Parameter {
  val field: String
  val value: Option[_]
  val typeName: String
  val description: Text
  private[radoc] val color: Color
}

private class SpecifiedColorParameter(val field: String,
                                      val value: Option[_],
                                      val typeName: String,
                                      val description: Text,
                                      private[radoc] val color: Color)
    extends Parameter

private class AutoColorParameter private (val field: String,
                                          val value: Option[_],
                                          val typeName: String,
                                          val description: Text)
    extends Parameter {

  def this(field: String, value: Any, typeName: String, description: Text) =
    this(field, Option(value), typeName, description)

  def this(field: String, value: Option[_], description: Text) =
    this(field,
         value,
         value.fold("Nothing")(_.getClass.getSimpleName),
         description)

  def this(field: String, value: Any, description: Text) =
    this(field, Option(value), description)

  def this(field: String, typeClass: Class[_], description: Text) =
    this(field, Option.empty, typeClass.getSimpleName, description)
  private[radoc] lazy val color: Color = ParameterColor.color
}

object Parameter {

  private[radoc] def apply(field: String,
                           value: Option[_],
                           typeName: String,
                           description: Text,
                           color: Color): Parameter =
    new SpecifiedColorParameter(field, value, typeName, description, color)

  def apply(field: String,
            value: Any,
            typeName: String,
            description: Text): Parameter =
    new AutoColorParameter(field, value, typeName, description)

  def apply(field: String,
            value: Option[_],
            typeName: String,
            description: Text): Parameter =
    new AutoColorParameter(field, value, typeName, description)

  def apply(field: String, value: Option[_], description: Text): Parameter =
    new AutoColorParameter(field, value, description)

  def apply(field: String, value: Any, description: Text): Parameter =
    new AutoColorParameter(field, value, description)
  def apply(field: String, typeClass: Class[_], description: Text): Parameter =
    new AutoColorParameter(field, typeClass, description)

}
