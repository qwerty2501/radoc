package net.qwerty2501.radoc

import scala.reflect.runtime.universe._

trait JsonHint {
  val parameterHint: ParameterHint
}

case class JsonObjectHint(parameterHint: ParameterHint,
                          childrenHints: Seq[JsonHint])
    extends JsonHint

case class JsonArrayHint(parameterHint: ParameterHint,
                         childrenHints: Seq[JsonHint])
    extends JsonHint

case class JsonValueHint(parameterHint: ParameterHint) extends JsonHint

class JsonBodyHint private (
    jsonHint: JsonHint,
    override val typeParameterMap: Map[String, Seq[Parameter]])
    extends BodyHint

object JsonBodyHint {

  def apply(jsonHint: JsonHint): JsonBodyHint =
    new JsonBodyHint(jsonHint, foldHints(jsonHint, Map()))

  private def foldHints(
      jsonHint: JsonHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] =
    jsonHint match {
      case jo: JsonObjectHint => foldObjectHints(jo, sourceMap)
      case _                  => sourceMap
    }

  private def getObjectHints(
      jsonObjectHint: JsonObjectHint): (String, Seq[Parameter]) =
    jsonObjectHint.parameterHint.parameter.typeName -> jsonObjectHint.childrenHints
      .map(_.parameterHint.parameter)

  private def foldObjectHints(
      jsonObjectHint: JsonObjectHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] = {

    jsonObjectHint.childrenHints.foldLeft(
      sourceMap + getObjectHints(jsonObjectHint)) { (sm, jh) =>
      foldHints(jh, sm)
    }
  }

  private def foldArrayHints(
      jsonArrayHint: JsonArrayHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] = {
    jsonArrayHint.childrenHints.foldLeft(sourceMap)((sm, hint) => {
      foldHints(hint, sm)
    })
  }
  private def getValueHint(jsonValueHint: JsonValueHint,
                           sourceMap: Map[String, Seq[Parameter]]): Parameter =
    jsonValueHint.parameterHint.parameter
  def apply[T](fieldModifier: FieldModifier = FieldModifier.Snake)(
      implicit ttag: TypeTag[T]): JsonBodyHint = {
    val accessors = typeOf[T].members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic => m.name.toString
    }
    new JsonBodyHint(JsonObjectHint(ParameterHint(Parameter("", "", Text()),
                                                  Essentiality.mandatory),
                                    Seq()),
                     Map())
  }

  def expected[T](expected: T, fieldModifier: FieldModifier)(
      implicit ttag: TypeTag[T]): JsonBodyHint = {
    new JsonBodyHint(JsonObjectHint(ParameterHint(Parameter("", "", Text()),
                                                  Essentiality.mandatory),
                                    Seq()),
                     Map())
  }
}
