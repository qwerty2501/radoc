package net.qwerty2501.radoc

import com.github.dwickern.macros.NameOf._
import io.circe._

private class JsonBodyHintMerger {
  private var unknownTypeCount: Int = 0
  def merge(json: Json, jsonBodyHint: JsonBodyHint): JsonBodyHint = {

    val (hint, typeParameterMap) = mergeParameterHints(
      json,
      jsonBodyHint.jsonHint,
      "",
      jsonBodyHint.typeParameterMap)
    new JsonBodyHint(hint, typeParameterMap)
  }

  private def mergeParameterHints(json: Json,
                                  jsonHint: JsonHint,
                                  field: String,
                                  typeParameterMap: Map[String, Seq[Parameter]])
    : (JsonHint, Map[String, Seq[Parameter]]) = {

    json.fold(
      mergeParameterHints(null.asInstanceOf[Any],
                          toValue(field, "Nothing", jsonHint),
                          typeParameterMap),
      jsonBoolean =>
        mergeParameterHints(jsonBoolean,
                            toValue(field, nameOf(Boolean), jsonHint),
                            typeParameterMap),
      jsonNumber =>
        mergeParameterHints(jsonNumber,
                            toValue(field, "Number", jsonHint),
                            typeParameterMap),
      jsonString =>
        mergeParameterHints(jsonString,
                            toValue(field, "String", jsonHint),
                            typeParameterMap),
      jsonArray =>
        mergeParameterHints(jsonArray,
                            toArray(field, jsonHint),
                            typeParameterMap),
      jsonObject =>
        mergeParameterHints(jsonObject,
                            toObject(field, jsonHint),
                            typeParameterMap)
    )

  }

  private def mergeParameterHints(jsonObject: JsonObject,
                                  jsonObjectHint: JsonObjectHint,
                                  typeParameterMap: Map[String, Seq[Parameter]])
    : (JsonObjectHint, Map[String, Seq[Parameter]]) = {

    val (newJsonObjectMap, newTypeParameterMap, newChildrenHints) =
      jsonObjectHint.childrenHintMap.foldLeft(
        (jsonObject.toMap, typeParameterMap, Map[String, JsonHint]())) {
        (args, hintTuple) =>
          val (j, t, hints) = args
          val (field, hint) = hintTuple

          if (j.isEmpty && hint.typeParameterHint.essentiality != Essentiality.Mandatory) {
            (j - field, t, hints)
          } else {
            val child = j.getOrElse(
              field,
              throw new AssertionError(
                s"expected field :$field but actual has don't have $field"))

            val (newHint, newTMap) =
              mergeParameterHints(child, hint, field, t)

            (j - field, newTMap, hints + (field -> newHint))
          }
      }

    val (nTm, nHints) =
      newJsonObjectMap.foldLeft((newTypeParameterMap, newChildrenHints)) {
        (args, child) =>
          {
            val (key, json) = child
            val (newTpm, newChs) = args
            val (rHint, rTpm) =
              mergeParameterHints(json, JsonNothingHint(), key, newTpm)
            (rTpm, newChs + (key -> rHint))
          }
      }

    (JsonObjectHint(jsonObjectHint.typeParameterHint, nHints),
     nTm + (jsonObjectHint.typeParameterHint.typeName -> nHints
       .map(f => f._2.typeParameterHint.toParameterHint(f._1).toParameter)
       .toSeq))

  }

  private def mergeParameterHints(
      jsonArray: Vector[Json],
      jsonArrayHint: JsonArrayHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonArrayHint, Map[String, Seq[Parameter]]) = {
    val (rHints, rTpm) =
      jsonArray.foldLeft((Seq[JsonHint](), typeParameterMap)) { (args, json) =>
        val (sourceHints, sourceTypeParameterMap) = args
        val (newHint, newTypeParameterMap) =
          mergeParameterHints(json,
                              jsonArrayHint.childrenTypeHint,
                              "",
                              sourceTypeParameterMap)

        if (jsonArray.head != json)
          (sourceHints, sourceTypeParameterMap)
        else
          (sourceHints :+ newHint, newTypeParameterMap)
      }

    val childTypeHint =
      if (rHints.isEmpty && !jsonArrayHint.childrenTypeHint
            .isInstanceOf[JsonNothingHint]) {
        jsonArrayHint.childrenTypeHint
      } else if (rHints.nonEmpty) {
        rHints.head
      } else {
        JsonValueHint(
          TypeParameterHint(
            "Nothing",
            jsonArrayHint.childrenTypeHint.typeParameterHint.description))
      }

    (JsonArrayHint(
       jsonArrayHint.typeParameterHint.copy(
         typeName = "[]" + childTypeHint.typeParameterHint.typeName),
       childTypeHint,
       rHints),
     rTpm)
  }

  private def mergeParameterHints(
      jsonNumber: JsonNumber,
      jsonValueHint: JsonValueHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonValueHint, Map[String, Seq[Parameter]]) =
    mergeParameterHints(jsonNumber.toDouble, jsonValueHint, typeParameterMap)

  private def mergeParameterHints(
      jsonValue: Any,
      jsonValueHint: JsonValueHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonValueHint, Map[String, Seq[Parameter]]) = {
    jsonValueHint.typeParameterHint.assert
      .assert(Option(jsonValue))
    (jsonValueHint, typeParameterMap)
  }

  private def toValue(field: String,
                      typeName: String,
                      jsonHint: JsonHint): JsonValueHint =
    jsonHint match {
      case jsonValueHint: JsonValueHint => jsonValueHint
      case _: JsonNothingHint =>
        JsonValueHint(TypeParameterHint(typeName))
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonValueHint) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toArray(field: String, jsonHint: JsonHint): JsonArrayHint =
    jsonHint match {
      case jsonArrayHint: JsonArrayHint => jsonArrayHint
      case _: JsonNothingHint =>
        JsonArrayHint(TypeParameterHint("[]", Text()), JsonNothingHint(), Seq())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonArrayHint) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toObject(field: String, jsonHint: JsonHint): JsonObjectHint =
    jsonHint match {
      case jsonObjectHint: JsonObjectHint => jsonObjectHint
      case _: JsonNothingHint =>
        JsonObjectHint(TypeParameterHint(objectName(), Text()), Map())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonObject) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def objectName(): String = {
    unknownTypeCount += 1
    "UnknownObject" + unknownTypeCount
  }

}
