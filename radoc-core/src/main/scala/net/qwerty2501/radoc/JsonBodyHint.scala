package net.qwerty2501.radoc

trait JsonHint {
  val typeParameterHint: TypeParameterHint
}

case class JsonObjectHint(typeParameterHint: TypeParameterHint,
                          childrenHintMap: Map[String, JsonHint])
    extends JsonHint

case class JsonArrayHint(typeParameterHint: TypeParameterHint,
                         childrenTypeHint: JsonHint,
                         childrenHints: Seq[JsonHint])
    extends JsonHint

case class JsonValueHint(typeParameterHint: TypeParameterHint) extends JsonHint

object JsonHint {}

private case class JsonNothingHint() extends JsonHint {
  override val typeParameterHint: TypeParameterHint =
    TypeParameterHint("Nothing")
}

private class JsonBodyHint private[radoc] (
    val jsonHint: JsonHint,
    override val typeParameterMap: Map[String, Seq[Parameter]])
    extends BodyHint

private object JsonBodyHint {

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
      typeParameterMap(
        jsonArrayHint.childrenTypeHint.typeParameterHint.typeName)
    JsonArrayHint(
      jsonArrayHint.typeParameterHint,
      jsonArrayHint.childrenTypeHint,
      jsonArrayHint.childrenHints.map(child =>
        recomposeChildren("", child, typeParameterMap, types))
    )
  }

  private def recompose(
      jsonObjectHint: JsonObjectHint,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonObjectHint = {
    val types =
      typeParameterMap(jsonObjectHint.typeParameterHint.typeName)
    val children = jsonObjectHint.childrenHintMap.map {
      case (key, jsonHint) =>
        (key, recomposeChildren(key, jsonHint, typeParameterMap, types))
    }
    JsonObjectHint(jsonObjectHint.typeParameterHint, children)
  }

  private def recomposeChildren(field: String,
                                jsonHint: JsonHint,
                                typeParameterMap: Map[String, Seq[Parameter]],
                                types: Seq[Parameter]): JsonHint = {
    val newJsonHint = recompose(jsonHint, typeParameterMap)
    val newParameter = types
      .find(_.field == field)
      .getOrElse(
        newJsonHint.typeParameterHint.toParameterHint(field).toParameter)
    val newTypeParameterHint =
      TypeParameterHint(newParameter.typeName,
                        newParameter.description,
                        newJsonHint.typeParameterHint.assert,
                        newJsonHint.typeParameterHint.essentiality)
    jsonHint match {
      case newJsonHint: JsonObjectHint =>
        JsonObjectHint(newTypeParameterHint, newJsonHint.childrenHintMap)

      case newArrayJsonHint: JsonArrayHint =>
        JsonArrayHint(newTypeParameterHint,
                      newArrayJsonHint.childrenTypeHint,
                      newArrayJsonHint.childrenHints)
      case _: JsonValueHint => JsonValueHint(newTypeParameterHint)
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
    jsonObjectHint.typeParameterHint.typeName -> jsonObjectHint.childrenHintMap
      .map(f => f._2.typeParameterHint.toParameterHint(f._1).toParameter)
      .toSeq

  private def foldObjectHints(
      jsonObjectHint: JsonObjectHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] = {

    jsonObjectHint.childrenHintMap.foldLeft(
      sourceMap + getObjectHints(jsonObjectHint)) { (sm, jh) =>
      foldHints(jh._2, sm)
    }
  }

  private def foldArrayHints(
      jsonArrayHint: JsonArrayHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] =
    foldHints(jsonArrayHint.childrenTypeHint, sourceMap)
  private def getValueHint(jsonValueHint: JsonValueHint,
                           sourceMap: Map[String, Seq[Parameter]]): Parameter =
    jsonValueHint.typeParameterHint.toParameterHint("").toParameter

}
