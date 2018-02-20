package net.qwerty2501.radoc

import io.circe.JsonObject
import org.scalatest._

class JsonBodyHintSpec extends FlatSpec with Matchers {

  it should "recompose same type" in {

    val strHint = JsonStringHint(SmallParameterHint(Text()))
    val obj = JsonObjectHint(
      TypeParameterHint("TestRoot", Text()),
      Map(
        "member1" -> JsonObjectHint(
          TypeParameterHint("TestObject1", Text()),
          Map(
            "id" -> JsonNumberHint(SmallParameterHint(Text())),
            "id2" -> JsonNumberHint(SmallParameterHint(Text()))
          )
        ),
        "member2" -> JsonObjectHint(
          TypeParameterHint("TestObject1", Text()),
          Map(
            "tt" -> JsonStringHint(SmallParameterHint(Text()))
          ))
      )
    )
    val hint = JsonBodyHint(obj)

    hint.jsonHint match {
      case root: JsonObjectHint =>
        root.typeParameterHint.typeName should be("TestRoot")
        root.childrenHintMap.foreach {
          case (field: String, member1: JsonObjectHint) if field == "member1" =>
            member1.typeParameterHint.typeName should be("TestObject1")

            member1.childrenHintMap.foreach {
              case (field: String, id: JsonNumberHint) if field == "id" =>
                id.typeParameterHint.typeName should be("Number")

              case (field: String, id2: JsonNumberHint) if field == "id2" =>
                id2.typeParameterHint.typeName should be("Number")

              case invalidId => fail("invalid Id:" + invalidId)
            }

          case (field: String, member2: JsonObjectHint) if field == "member2" =>
            member2.typeParameterHint.typeName should be("TestObject1")
            member2.childrenHintMap.foreach {
              case (field: String, tt: JsonStringHint) if field == "tt" =>
                tt.typeParameterHint.typeName should be("String")
              case invalidTt => fail("invalid tt:" + invalidTt)
            }

          case invalidMember => fail("invalid member:" + invalidMember)
        }

      case invalidRoot => fail("invalid root:" + invalidRoot)
    }

    hint.typeParameterMap.foreach {
      case ("TestRoot", v) =>
        v.length should be(2)
        v.map(_.field).distinct.length should be(v.length)
        v.foreach {
          case p: Parameter if p.field == "member1" =>
            p.typeName should be("TestObject1")

          case p: Parameter if p.field == "member2" =>
            p.typeName should be("TestObject1")
        }

      case ("TestObject1", v) =>
        v.length should be(1)
        v.map(_.field).distinct.length should be(v.length)
        v.head.field == "tt"
        v.head.typeName == "String"

      case invalidType => fail("invalid type:" + invalidType)
    }
  }
}
