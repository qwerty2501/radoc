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
                                  jsonObjectHint: JsonHint.Object,
                                  typeParameterMap: Map[String, Seq[Parameter]])
    : (JsonHint.Object, Map[String, Seq[Parameter]]) = {

    val (newJsonObjectMap, newTypeParameterMap, newChildrenHints) =
      jsonObjectHint.childrenHintMap.foldLeft(
        (jsonObject.toMap, typeParameterMap, Map[String,JsonHint]())) { (args, hintTuple) =>
        val (j, t, hints) = args
        val (field,hint) = hintTuple

        if (j.isEmpty && hint.typeParameterHint.essentiality != Essentiality.Mandatory) {
          (j - field, t, hints)
        } else {
          val child = j.getOrElse(
            field,
            throw new AssertionError(
              s"expected field :$field but actual has don't have $field"))

          val (newHint, newTMap) =
            mergeParameterHints(child, hint, field, t)

          (j - field, newTMap, hints + (field ->newHint))
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
            (rTpm, newChs + (key->rHint))
          }
      }

    (JsonHint.Object(jsonObjectHint.typeParameterHint, nHints),
     nTm + (jsonObjectHint.typeParameterHint.typeName -> nHints.map(f=>
       f._2.typeParameterHint.toParameterHint(f._1).toParameter).toSeq))

  }

  private def mergeParameterHints(
      jsonArray: Vector[Json],
      jsonArrayHint: JsonHint.Array,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonHint.Array, Map[String, Seq[Parameter]]) = {
    val (rHints, rTpm) =
      jsonArray.foldLeft((Seq[JsonHint](), typeParameterMap)) { (args, json) =>
        val (hints, tpm) = args
        val (hint, newTpm) =
          mergeParameterHints(json, jsonArrayHint.childrenTypeHint, "", tpm)

        if (jsonArrayHint.childrenTypeHint.isInstanceOf[JsonNothingHint])
          (hints, tpm)
        else
          (hints :+ hint, newTpm)
      }

    val childTypeHint =
      if (rHints.isEmpty && !jsonArrayHint.childrenTypeHint
            .isInstanceOf[JsonNothingHint]) {
        jsonArrayHint.childrenTypeHint
      } else if (rHints.lengthCompare(1) == 0) {
        rHints.head
      } else {
        JsonHint.Value(
          TypeParameterHint(
            "(" + rHints.map(_.typeParameterHint.typeName).mkString("|") + ")",
            jsonArrayHint.childrenTypeHint.typeParameterHint.description))
      }

    (JsonHint.Array(jsonArrayHint.typeParameterHint.copy(
                     typeName = "[]" + childTypeHint.typeParameterHint.typeName),
                   childTypeHint,
                   rHints),
     rTpm)
  }

  private def mergeParameterHints(
      jsonNumber: JsonNumber,
      jsonValueHint: JsonHint.Value,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonHint.Value, Map[String, Seq[Parameter]]) =
    mergeParameterHints(jsonNumber.toDouble, jsonValueHint, typeParameterMap)

  private def mergeParameterHints(
      jsonValue: Any,
      jsonValueHint: JsonHint.Value,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonHint.Value, Map[String, Seq[Parameter]]) = {
    jsonValueHint.typeParameterHint.assert
      .assert(Option(jsonValue))
    (jsonValueHint, typeParameterMap)
  }

  private def toValue(field: String,
                      typeName: String,
                      jsonHint: JsonHint): JsonHint.Value =
    jsonHint match {
      case jsonValueHint: JsonHint.Value => jsonValueHint
      case _: JsonNothingHint =>
        JsonHint.Value(TypeParameterHint( typeName))
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonHint.Value) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toArray(field: String, jsonHint: JsonHint): JsonHint.Array =
    jsonHint match {
      case jsonArrayHint: JsonHint.Array => jsonArrayHint
      case _: JsonNothingHint =>
        JsonHint.Array(TypeParameterHint( "[]", Text()),
                      JsonNothingHint(),
                      Seq())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonHint.Array) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toObject(field: String, jsonHint: JsonHint): JsonHint.Object =
    jsonHint match {
      case jsonObjectHint: JsonHint.Object => jsonObjectHint
      case _: JsonNothingHint =>
        JsonHint.Object(TypeParameterHint( objectName(), Text()), Map())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonObject) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def objectName(): String = {
    unknownTypeCount += 1
    "Unknown Object " + unknownTypeCount
  }

}
