package net.qwerty2501.radoc

trait JsonHint {
  val typeParameterHint: TypeParameterHint
}


object JsonHint{
  case class Object(typeParameterHint: TypeParameterHint,
                            childrenHintMap: Map[String,JsonHint])
    extends JsonHint

  case class Array(typeParameterHint: TypeParameterHint,
                           childrenTypeHint: JsonHint,
                           childrenHints: Seq[JsonHint])
    extends JsonHint

  case class Value(typeParameterHint: TypeParameterHint) extends JsonHint
}



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
      case jsonObjectHint: JsonHint.Object =>
        recompose(jsonObjectHint, typeParameterMap)
      case jsonArrayHint: JsonHint.Array =>
        recompose(jsonArrayHint, typeParameterMap)
      case jsonValue: JsonHint.Value => jsonValue
    }
  }

  private def recompose(
      jsonArrayHint: JsonHint.Array,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonHint.Array = {
    val types =
      typeParameterMap(jsonArrayHint.childrenTypeHint.typeParameterHint.typeName)
    JsonHint.Array(jsonArrayHint.typeParameterHint,
                  jsonArrayHint.childrenTypeHint,
                  jsonArrayHint.childrenHints.map(child =>
                    recomposeChildren("",child, typeParameterMap, types)))
  }

  private def recompose(
      jsonObjectHint: JsonHint.Object,
      typeParameterMap: Map[String, Seq[Parameter]]): JsonHint.Object = {
    val types =
      typeParameterMap(jsonObjectHint.typeParameterHint.typeName)
    val children = jsonObjectHint.childrenHintMap.map {
      case (key,jsonHint)=>
        (key,recomposeChildren(key,jsonHint, typeParameterMap, types))
    }
    JsonHint.Object(jsonObjectHint.typeParameterHint, children)
  }

  private def recomposeChildren(field:String,
                                 jsonHint: JsonHint,
                                typeParameterMap: Map[String, Seq[Parameter]],
                                types: Seq[Parameter]): JsonHint = {
    val newJsonHint = recompose(jsonHint, typeParameterMap)
    val newParameter = types
      .find(_.field == field)
      .getOrElse(newJsonHint.typeParameterHint.toParameterHint(field).toParameter)
    val newTypeParameterHint =
      TypeParameterHint(
                    newParameter.typeName,
                    newParameter.description,
                    newJsonHint.typeParameterHint.assert,
                    newJsonHint.typeParameterHint.essentiality)
    jsonHint match {
      case newJsonHint: JsonHint.Object =>
        JsonHint.Object(newTypeParameterHint, newJsonHint.childrenHintMap)

      case newArrayJsonHint: JsonHint.Array =>
        JsonHint.Array(newTypeParameterHint,
                      newArrayJsonHint.childrenTypeHint,
                      newArrayJsonHint.childrenHints)
      case _: JsonHint.Value => JsonHint.Value(newTypeParameterHint)
    }
  }

  private def foldHints(
      jsonHint: JsonHint,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] =
    jsonHint match {
      case jo: JsonHint.Object => foldObjectHints(jo, sourceMap)
      case _                  => sourceMap
    }

  private def getObjectHints(
      jsonObjectHint: JsonHint.Object): (String, Seq[Parameter]) =
    jsonObjectHint.typeParameterHint.typeName -> jsonObjectHint.childrenHintMap
      .map(f => f._2.typeParameterHint.toParameterHint(f._1).toParameter).toSeq

  private def foldObjectHints(
      jsonObjectHint: JsonHint.Object,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] = {

    jsonObjectHint.childrenHintMap.foldLeft(
      sourceMap + getObjectHints(jsonObjectHint)) { (sm, jh) =>
      foldHints(jh._2, sm)
    }
  }

  private def foldArrayHints(
      jsonArrayHint: JsonHint.Array,
      sourceMap: Map[String, Seq[Parameter]]): Map[String, Seq[Parameter]] =
    foldHints(jsonArrayHint.childrenTypeHint, sourceMap)
  private def getValueHint(jsonValueHint: JsonHint.Value,
                           sourceMap: Map[String, Seq[Parameter]]): Parameter =
    jsonValueHint.typeParameterHint.toParameterHint("").toParameter

}
