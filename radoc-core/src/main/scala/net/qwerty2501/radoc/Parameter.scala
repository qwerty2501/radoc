package net.qwerty2501.radoc

import scala.reflect._

class Parameter private (val field: String,
                         val value: Option[_],
                         val typeName: String,
                         val description: Text) {

  def this(field: String, value: Option[_], cs: Class[_], description: Text) =
    this(field,
         value,
         rootMirror.classSymbol(cs).name.toTypeName.toString,
         description)

  lazy val color: Color = ParameterColor.color
}

object Parameter {

  private def apply(field: String,
                    value: Option[_],
                    typeName: String,
                    description: Text,
                    color: Color): Parameter =
    new Parameter(field, value, typeName, description)

  def apply(field: String,
            value: Option[_],
            typeName: String,
            description: Text): Parameter =
    new Parameter(field, value, typeName, description)

  def apply[T](field: String, value: T, description: Text)(
      implicit ct: ClassTag[T]): Parameter =
    new Parameter(field, Option(value), ct.runtimeClass, description)

  def apply(field: String, description: Text): Parameter =
    new Parameter(field, Option.empty, "", description)

  def apply(field: String,
            value: Option[_],
            tte: Class[_],
            description: Text): Parameter =
    new Parameter(field, value, tte, description)

}
