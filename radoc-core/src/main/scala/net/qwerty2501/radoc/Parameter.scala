package net.qwerty2501.radoc

import scala.reflect._

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

  def this(field: String, value: Any, tte: Class[_], description: Text) =
    this(field, value, tte.getSimpleName, description)

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

  def apply[T: ClassTag](field: String, value: T, description: Text)(
      implicit ct: ClassTag[T]): Parameter =
    new Parameter(field, value, ct.runtimeClass, description)
  def apply[T: ClassTag](field: String, description: Text)(
      implicit ct: ClassTag[T]): Parameter =
    new Parameter(field, "", ct.runtimeClass, description)

  def apply(field: String,
            value: Any,
            tte: Class[_],
            description: Text): Parameter =
    new Parameter(field, value, tte, description)

}
