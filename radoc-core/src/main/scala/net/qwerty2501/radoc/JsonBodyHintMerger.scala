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
                                  jsonHint: JsonHint[_],
                                  field: String,
                                  typeParameterMap: Map[String, Seq[Parameter]])
    : (JsonHint[_], Map[String, Seq[Parameter]]) = {

    json.fold(
      mergeParameterHints(null, toNull(field, jsonHint), typeParameterMap),
      jsonBoolean =>
        mergeParameterHints(jsonBoolean,
                            toBoolean(field, jsonHint),
                            typeParameterMap),
      jsonNumber =>
        mergeParameterHints(jsonNumber,
                            toNumber(field, jsonHint),
                            typeParameterMap),
      jsonString =>
        mergeParameterHints(jsonString,
                            toJsonString(field, jsonHint),
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
    jsonObjectHint.typeParameterHint.assert(Option(jsonObject))
    val (newJsonObjectMap, newTypeParameterMap, newChildrenHints) =
      jsonObjectHint.childrenHintMap.foldLeft(
        (jsonObject.toMap, typeParameterMap, Map[String, JsonHint[_]]())) {
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
      jsonArray: Seq[Json],
      jsonArrayHint: JsonArrayHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonArrayHint, Map[String, Seq[Parameter]]) = {
    jsonArrayHint.typeParameterHint.assert(Option(jsonArray))
    val (checkedArray, checkedHints, checkedTypeParameterMap) =
      jsonArrayHint.childrenHints.zipWithIndex
        .foldLeft((jsonArray, Seq[JsonHint[_]](), typeParameterMap)) {
          (args, tHint) =>
            args match {
              case (jsonArray: Seq[Json],
                    hints: Seq[JsonHint[_]],
                    typeParameterMap: Map[String, Seq[Parameter]]) =>
                val (hint, index) = tHint
                val compareResult = jsonArray
                  .lengthCompare(index) > 0
                if (hint.typeParameterHint.essentiality == Essentiality.Mandatory && !compareResult) {
                  throw new AssertionError(
                    "The hint essentiality is mandatory, but actual json array is not enough length.")
                } else if (compareResult) {
                  val targetJson = jsonArray(index)
                  val (newJsonHint, newTypeParameterMap) =
                    mergeParameterHints(targetJson, hint, "", typeParameterMap)
                  (jsonArray.drop(index),
                   hints :+ newJsonHint,
                   newTypeParameterMap)
                } else {
                  (jsonArray, hints, typeParameterMap)
                }

            }
        }
    val (rHints, rTpm) =
      checkedArray.foldLeft((checkedHints, checkedTypeParameterMap)) {
        (args, json) =>
          val (sourceHints, sourceTypeParameterMap) = args
          val (newHint, newTypeParameterMap) =
            mergeParameterHints(json,
                                JsonNothingHint(),
                                "",
                                sourceTypeParameterMap)

          if (checkedArray.head != json || sourceHints.nonEmpty)
            (sourceHints, sourceTypeParameterMap)
          else
            (sourceHints :+ newHint, newTypeParameterMap)
      }

    (JsonArrayHint(jsonArrayHint.typeParameterHint.copy(
                     typeName = "[]" + rHints.headOption.fold("")(
                       _.typeParameterHint.typeName)),
                   rHints),
     rTpm)
  }

  private def mergeParameterHints(
      jsonNumber: JsonNumber,
      jsonNumberHint: JsonNumberHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonNumberHint, Map[String, Seq[Parameter]]) = {
    jsonNumberHint.typeParameterHint.assert(Option(jsonNumber.toDouble))
    (jsonNumberHint, typeParameterMap)
  }
  private def mergeParameterHints(
      jsonValue: String,
      jsonValueHint: JsonStringHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonStringHint, Map[String, Seq[Parameter]]) = {
    jsonValueHint.typeParameterHint.assert(Option(jsonValue))
    (jsonValueHint, typeParameterMap)
  }

  private def mergeParameterHints(
      jsonValue: Null,
      jsonValueHint: JsonNullHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonNullHint, Map[String, Seq[Parameter]]) = {
    jsonValueHint.typeParameterHint.assert(Option(jsonValue))
    (jsonValueHint, typeParameterMap)
  }

  private def mergeParameterHints(
      jsonValue: Boolean,
      jsonValueHint: JsonBooleanHint,
      typeParameterMap: Map[String, Seq[Parameter]]
  ): (JsonBooleanHint, Map[String, Seq[Parameter]]) = {
    jsonValueHint.typeParameterHint.assert(Option(jsonValue))
    (jsonValueHint, typeParameterMap)
  }

  private def toNumber(field: String, jsonHint: JsonHint[_]): JsonNumberHint =
    jsonHint match {
      case jsonNumberHint: JsonNumberHint => jsonNumberHint
      case _: JsonNothingHint =>
        new JsonNumberHint(SmallParameterHint[Double]())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonNumber) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toBoolean(field: String, jsonHint: JsonHint[_]): JsonBooleanHint =
    jsonHint match {
      case jsonBooleanHint: JsonBooleanHint => jsonBooleanHint
      case _: JsonNothingHint =>
        new JsonBooleanHint(SmallParameterHint[Boolean]())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonBooleanHint) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toJsonString(field: String,
                           jsonHint: JsonHint[_]): JsonStringHint =
    jsonHint match {
      case jsonStringHint: JsonStringHint => jsonStringHint
      case _: JsonNothingHint =>
        new JsonStringHint(SmallParameterHint[String]())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonStringHint) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toString(field: String, jsonHint: JsonHint[_]): JsonBooleanHint =
    jsonHint match {
      case jsonBooleanHint: JsonBooleanHint => jsonBooleanHint
      case _: JsonNothingHint =>
        new JsonBooleanHint(SmallParameterHint[Boolean]())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonBooleanHint) + " but actual:" + actual.getClass.getSimpleName)
    }
  private def toNull(field: String, jsonHint: JsonHint[_]): JsonNullHint =
    jsonHint match {
      case jsonNullHint: JsonNullHint => jsonNullHint
      case _: JsonNothingHint =>
        new JsonNullHint(SmallParameterHint[Null]())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonNullHint) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toArray(field: String, jsonHint: JsonHint[_]): JsonArrayHint =
    jsonHint match {
      case jsonArrayHint: JsonArrayHint => jsonArrayHint
      case _: JsonNothingHint =>
        JsonArrayHint(TypeParameterHint("[]", Text()), Seq())
      case actual =>
        throw new AssertionError(
          "expected:" + nameOf(JsonArrayHint) + " but actual:" + actual.getClass.getSimpleName)
    }

  private def toObject(field: String, jsonHint: JsonHint[_]): JsonObjectHint =
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
