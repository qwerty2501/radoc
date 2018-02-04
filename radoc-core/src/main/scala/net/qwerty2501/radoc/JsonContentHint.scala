package net.qwerty2501.radoc

import scala.reflect.runtime.universe._
trait JsonHint {
  val parameterHint: ParameterHint
}

case class JsonObjectHint(parameterHint: ParameterHint, children: Seq[JsonHint])
    extends JsonHint

case class JsonArrayHint(parameterHint: ParameterHint, children: Seq[JsonHint])
    extends JsonHint

case class JsonValueHint(parameterHint: ParameterHint) extends JsonHint

case class JsonReflectionHint(root: Map[String, Text],
                              children: Map[(Class[_], String), Text])

object JsonHint {
  def apply(hint: Class[_],
            fieldModifier: FieldModifier,
            descriptionMap: Map[String, Text]): JsonHint = {
    val rm = scala.reflect.runtime.currentMirror
    val accessors = rm.classSymbol(hint).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m.asType.name
    }
    JsonObjectHint(ParameterHint(Parameter("", "", Text())), Seq())
  }
}
