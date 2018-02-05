package net.qwerty2501.radoc

import scala.reflect.runtime.universe._
trait JsonBodyHint {
  val parameterHint: ParameterHint
}

case class JsonObjectBodyHint(parameterHint: ParameterHint,
                              children: Seq[JsonBodyHint])
    extends JsonBodyHint

case class JsonArrayBodyHint(parameterHint: ParameterHint,
                             children: Seq[JsonBodyHint])
    extends JsonBodyHint

case class JsonValueBodyHint(parameterHint: ParameterHint) extends JsonBodyHint

case class JsonReflectionHint(root: Map[String, Text],
                              children: Map[(Class[_], String), Text])

object JsonBodyHint {

  def apply[T](fieldModifier: FieldModifier, descriptionMap: Map[String, Text])(
      implicit ttag: TypeTag[T]): JsonBodyHint = {
    val accessors = typeOf[T].members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m.asType.name
    }
    JsonObjectBodyHint(
      ParameterHint(Parameter("", "", Text()), Essentiality.mandatory),
      Seq())
  }
}
