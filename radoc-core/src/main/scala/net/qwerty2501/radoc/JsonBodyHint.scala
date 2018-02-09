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

  def apply(jsonHint: JsonHint): JsonBodyHint = {
    val typeParameterMap = foldHints(jsonHint, Map())
    new JsonBodyHint(recompose(jsonHint, typeParameterMap), typeParameterMap)
  }

  def apply[T: TypeTag](
      fieldModifier: FieldModifier = FieldModifier.Snake): JsonBodyHint = {
    val accessors = typeOf[T].members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic =>
        m.returnType.typeSymbol
    }

    new JsonBodyHint(JsonObjectHint(ParameterHint(Parameter("", "", Text()),
                                                  Essentiality.Mandatory),
                                    Seq()),
                     Map())
  }

  private final val seqTypeName = classOf[Seq[_]].getName

  def expected[T: TypeTag](
      expected: T,
      fieldModifier: FieldModifier = FieldModifier.Snake): JsonBodyHint = {
    new JsonBodyHint(JsonObjectHint(ParameterHint(Parameter("", "", Text()),
                                                  Essentiality.Mandatory),
                                    Seq()),
                     Map())
  }

  /*
  private def getFromTypeHint(
      symbol: MethodSymbol,
      fieldModifier: FieldModifier = FieldModifier.Snake,
      generateAssertHandler: (ParameterHint) => Unit): JsonHint = {
    val valueType = symbol.returnType
    val typeName =
      if (valueType.baseClasses.exists(_.asClass.fullName == seqTypeName))
        "[]" + valueType.typeArgs.head.toString
      else valueType.toString

    if (valueType.typeSymbol.asClass.isPrimitive) {}

  }
   */
  private def recompose(
      jsonHint: JsonHint,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonHint = {

    jsonHint match {
      case jsonObjectHint: JsonObjectHint =>
        recompose(jsonObjectHint, typeParameterMap)
      case jsonArrayHint: JsonArrayHint =>
        recompose(jsonArrayHint, typeParameterMap)
    }
  }

  private def recompose(
      jsonArrayHint: JsonArrayHint,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonArrayHint = {
    JsonArrayHint(jsonArrayHint.parameterHint,
                  jsonArrayHint.childrenHints.map(child =>
                    recompose(child, typeParameterMap)))
  }

  private def recompose(
      jsonObjectHint: JsonObjectHint,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonObjectHint = {
    val types =
      typeParameterMap(jsonObjectHint.parameterHint.parameter.typeName)
    val childlen = jsonObjectHint.childrenHints.map { jsonHint =>
      recomposeChildren(jsonHint, typeParameterMap, types)
    }
    JsonObjectHint(jsonObjectHint.parameterHint, childlen)
  }

  private def recomposeChildren(jsonHint: JsonHint,
                                typeParameterMap: Map[String, Seq[Parameter]],
                                types: Seq[Parameter]): JsonHint = {
    val newJsonHint = recompose(jsonHint, typeParameterMap)
    val newParameter = types
      .find(_.field == newJsonHint.parameterHint.parameter.field)
      .getOrElse(newJsonHint.parameterHint.parameter)
    val newParameterHint =
      ParameterHint(newParameter,
                    newJsonHint.parameterHint.assert,
                    newJsonHint.parameterHint.essentiality)
    jsonHint match {
      case newJsonObjectHint: JsonObjectHint =>
        JsonObjectHint(newParameterHint, newJsonObjectHint.childrenHints)

      case newArrayJsonObjectHint: JsonArrayHint =>
        JsonArrayHint(newParameterHint, newArrayJsonObjectHint.childrenHints)
      case _: JsonValueHint => JsonValueHint(newParameterHint)
    }
  }

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

}
