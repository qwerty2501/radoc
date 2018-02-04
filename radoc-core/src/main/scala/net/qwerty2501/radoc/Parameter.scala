package net.qwerty2501.radoc

case class ParameterValue(value: Any) {

  override def hashCode(): Int = if (value == null) 0 else value.hashCode
  override def equals(o: scala.Any): Boolean = o == value

  override def toString: String = if (value == null) "null" else value.toString
}

case class Parameter private (field: String,
                              value: ParameterValue,
                              typeName: String,
                              description: Text,
                              private[radoc] val color: Color) {

  def this(field: String, value: Any, typeName: String, description: Text) =
    this(field,
         ParameterValue(value),
         typeName,
         description,
         ParameterColor.color())
  def this(field: String,
           value: Any,
           typeNames: Seq[String],
           description: Text) =
    this(field, value, typeNames.mkString(" | "), description)

  def this(field: String, value: Any, valueType: Class[_], description: Text) =
    this(field, value, valueType.getSimpleName, description)

  def this(field: String, value: Any, description: Text) =
    this(field,
         value,
         if (value != null) value.getClass else AnyRef.getClass,
         description)

}

object Parameter {

  private def apply(field: String,
                    value: Any,
                    typeName: String,
                    description: Text,
                    color: Color): Parameter =
    new Parameter(field, value, typeName, description)

  def apply(field: String,
            value: Any,
            typeName: String,
            description: Text): Parameter =
    new Parameter(field, value, typeName, description)

  def apply(field: String, value: Any, description: Text): Parameter =
    new Parameter(field, value, value.getClass, description)

  def apply(field: String,
            value: Any,
            valueType: Class[_],
            description: Text): Parameter =
    new Parameter(field, value, valueType, description)

  def apply(field: String,
            value: Any,
            typeNames: Seq[String],
            description: Text) =
    new Parameter(field, value, typeNames, description)
}
