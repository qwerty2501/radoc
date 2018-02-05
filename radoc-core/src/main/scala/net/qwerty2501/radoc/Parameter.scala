package net.qwerty2501.radoc

import scala.reflect.runtime.universe._
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

  def this(field: String, value: Any, tte: Type, description: Text) =
    this(field, value, tte.typeSymbol.name.toString, description)

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

  def apply[T](field: String, value: T, description: Text)(
      implicit ttc: TypeTag[T]): Parameter =
    new Parameter(field, value, typeOf[T], description)
  def apply[T](field: String, description: Text)(
      implicit ttc: TypeTag[T]): Parameter =
    new Parameter(field, "", typeOf[T], description)

  def apply(field: String,
            value: Any,
            tte: Type,
            description: Text): Parameter =
    new Parameter(field, value, tte, description)

  def apply(field: String,
            value: Any,
            typeNames: Seq[String],
            description: Text) =
    new Parameter(field, value, typeNames, description)
}
