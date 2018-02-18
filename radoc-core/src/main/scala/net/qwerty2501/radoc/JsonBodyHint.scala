package net.qwerty2501.radoc

trait JsonHint {
  val parameterHint: ParameterHint
}

case class JsonObjectHint(parameterHint: ParameterHint,
                          childrenHints: Seq[JsonHint])
    extends JsonHint

case class JsonArrayHint(parameterHint: ParameterHint,
                         childrenTypeHint: JsonHint,
                         childrenHints: Seq[JsonHint])
    extends JsonHint

case class JsonValueHint(parameterHint: ParameterHint) extends JsonHint

private case class JsonNothingHint() extends JsonHint {
  override val parameterHint: ParameterHint =
    ParameterHint("", "Nothing", Text())
}

class JsonBodyHint private[radoc] (
    val jsonHint: JsonHint,
    override val typeParameterMap: Map[String, Seq[Parameter]])
    extends BodyHint

object JsonBodyHint {

  private[radoc] def apply() = new JsonBodyHint(JsonNothingHint(), Map())
  def apply(jsonHint: JsonHint): JsonBodyHint = {
    val typeParameterMap = foldHints(jsonHint, Map())
    new JsonBodyHint(recompose(jsonHint, typeParameterMap), typeParameterMap)
  }

  private def recompose(
      jsonHint: JsonHint,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonHint = {

    jsonHint match {
      case jsonObjectHint: JsonObjectHint =>
        recompose(jsonObjectHint, typeParameterMap)
      case jsonArrayHint: JsonArrayHint =>
        recompose(jsonArrayHint, typeParameterMap)
      case jsonValue: JsonValueHint => jsonValue
    }
  }

  private def recompose(
      jsonArrayHint: JsonArrayHint,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonArrayHint = {
    val types =
      typeParameterMap(jsonArrayHint.childrenTypeHint.parameterHint.typeName)
    JsonArrayHint(jsonArrayHint.parameterHint,
                  jsonArrayHint.childrenTypeHint,
                  jsonArrayHint.childrenHints.map(child =>
                    recomposeChildren(child, typeParameterMap, types)))
  }

  private def recompose(
      jsonObjectHint: JsonObjectHint,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonObjectHint = {
    val types =
      typeParameterMap(jsonObjectHint.parameterHint.typeName)
    val children = jsonObjectHint.childrenHints.map { jsonHint =>
      recomposeChildren(jsonHint, typeParameterMap, types)
    }
    JsonObjectHint(jsonObjectHint.parameterHint, children)
  }

  private def recomposeChildren(jsonHint: JsonHint,
                                typeParameterMap: Map[String, Seq[Parameter]],
                                types: Seq[Parameter]): JsonHint = {
    val newJsonHint = recompose(jsonHint, typeParameterMap)
    val newParameter = types
      .find(_.field == newJsonHint.parameterHint.field)
      .getOrElse(newJsonHint.parameterHint.toParameter)
    val newParameterHint =
      ParameterHint(newParameter.field,
                    newParameter.typeName,
                    newParameter.description,
                    newJsonHint.parameterHint.assert,
                    newJsonHint.parameterHint.essentiality)
    jsonHint match {
      case newJsonObjectHint: JsonObjectHint =>
        JsonObjectHint(newParameterHint, newJsonObjectHint.childrenHints)

      case newArrayJsonObjectHint: JsonArrayHint =>
        JsonArrayHint(newParameterHint,
                      newArrayJsonObjectHint.childrenTypeHint,
                      newArrayJsonObjectHint.childrenHints)
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
    jsonObjectHint.parameterHint.typeName -> jsonObjectHint.childrenHints
      .map(_.parameterHint.toParameter)

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
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] =
    foldHints(jsonArrayHint.childrenTypeHint, sourceMap)
  private def getValueHint(jsonValueHint: JsonValueHint,
                           sourceMap: Map[String, Seq[Parameter]]): Parameter =
    jsonValueHint.parameterHint.toParameter

}
