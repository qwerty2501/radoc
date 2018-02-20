package net.qwerty2501.radoc

import io.circe._

trait JsonHint[T] {
  val typeParameterHint: TypeParameterHint[T]
}

case class JsonObjectHint(typeParameterHint: TypeParameterHint[JsonObject],
                          childrenHintMap: Map[String, JsonHint[_]])
    extends JsonHint[JsonObject]

case class JsonArrayHint(typeParameterHint: TypeParameterHint[Seq[Json]],
                         childrenHints: Seq[JsonHint[_]])
    extends JsonHint[Seq[Json]] {}

trait JsonValueHint[T] extends JsonHint[T] {
  val typeParameterHint: TypeParameterHint[T]
}

class JsonNumberHint private[radoc] (
    override val typeParameterHint: TypeParameterHint[Double])
    extends JsonValueHint[Double] {
  def this(parameterHint: SmallParameterHint[Double]) =
    this(parameterHint.toTypeParameterHint("Number"))
}

object JsonNumberHint {
  def apply(parameterHint: SmallParameterHint[Double]): JsonNumberHint =
    new JsonNumberHint(parameterHint)
}

class JsonStringHint private[radoc] (
    override val typeParameterHint: TypeParameterHint[String])
    extends JsonValueHint[String] {

  def this(parameterHint: SmallParameterHint[String]) =
    this(parameterHint.toTypeParameterHint("String"))
}

object JsonStringHint {
  def apply(parameterHint: SmallParameterHint[String]): JsonStringHint =
    new JsonStringHint(parameterHint)
}

class JsonBooleanHint private[radoc] (
    override val typeParameterHint: TypeParameterHint[Boolean])
    extends JsonValueHint[Boolean] {
  def this(parameterHint: SmallParameterHint[Boolean]) =
    this(parameterHint.toTypeParameterHint("Boolean"))
}

object JsonBooleanHint {
  def apply(parameterHint: SmallParameterHint[Boolean]): JsonBooleanHint =
    new JsonBooleanHint(parameterHint)
}

class JsonNullHint private[radoc] (
    override val typeParameterHint: TypeParameterHint[Null])
    extends JsonValueHint[Null] {
  def this(parameterHint: SmallParameterHint[Null]) =
    this(parameterHint.toTypeParameterHint("Nothing"))

}

object JsonNullHint {
  def apply(parameterHint: SmallParameterHint[Null]): JsonNullHint =
    new JsonNullHint(parameterHint)
}

private case class JsonNothingHint() extends JsonHint[Nothing] {
  override val typeParameterHint: TypeParameterHint[Nothing] =
    TypeParameterHint("Nothing")
}

private class JsonBodyHint private[radoc] (
    val jsonHint: JsonHint[_],
    override val typeParameterMap: Map[String, Seq[Parameter]])
    extends BodyHint

private object JsonBodyHint {

  private[radoc] def apply() = new JsonBodyHint(JsonNothingHint(), Map())

  def apply(jsonHint: JsonHint[_]): JsonBodyHint = {
    val typeParameterMap = foldHints(jsonHint, Map())
    new JsonBodyHint(jsonHint, typeParameterMap)
  }

  private def foldHints(
      jsonHint: JsonHint[_],
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] =
    jsonHint match {
      case jo: JsonObjectHint => foldObjectHints(jo, sourceMap)
      case _                  => sourceMap
    }

  private def getObjectHints(
      jsonObjectHint: JsonObjectHint): (String, Seq[Parameter]) =
    jsonObjectHint.typeParameterHint.typeName -> jsonObjectHint.childrenHintMap
      .map(f => f._2.typeParameterHint.toParameterHint(f._1).toParameter)
      .toSeq

  private def foldObjectHints(
      jsonObjectHint: JsonObjectHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] = {

    jsonObjectHint.childrenHintMap.foldLeft(
      sourceMap + getObjectHints(jsonObjectHint)) { (sm, jh) =>
      if (jh._2.typeParameterHint.essentiality == Essentiality.Excluded) {
        sm
      } else {
        foldHints(jh._2, sm)
      }
    }
  }

  private def foldArrayHints(
      jsonArrayHint: JsonArrayHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] =
    jsonArrayHint.childrenHints.foldLeft(sourceMap) {
      (childHint, argSourceMap) =>
        foldHints(argSourceMap, childHint)
    }

  private def getValueHint(jsonValueHint: JsonValueHint[_],
                           sourceMap: Map[String, Seq[Parameter]]): Parameter =
    jsonValueHint.typeParameterHint.toParameterHint("").toParameter

}
