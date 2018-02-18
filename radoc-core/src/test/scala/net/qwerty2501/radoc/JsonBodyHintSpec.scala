package net.qwerty2501.radoc

import org.scalatest._

class JsonBodyHintSpec extends FlatSpec with Matchers {

  it should "recompose same type" in {
    val hint = JsonBodyHint(
      JsonObjectHint(
        ParameterHint("", "TestRoot", Text()),
        Seq(
          JsonObjectHint(
            ParameterHint("member1", "TestObject1", Text()),
            Seq(
              JsonValueHint(ParameterHint("id", "Int", Text())),
              JsonValueHint(ParameterHint("id2", "Double", Text()))
            )
          ),
          JsonObjectHint(ParameterHint("member2", "TestObject1", Text()),
                         Seq(
                           JsonValueHint(ParameterHint("tt", "String", Text()))
                         ))
        )
      ))

    hint.jsonHint match {
      case root: JsonObjectHint if root.parameterHint.field == "" =>
        root.parameterHint.typeName should be("TestRoot")
        root.childrenHints.foreach {
          case member1: JsonObjectHint
              if member1.parameterHint.field == "member1" =>
            member1.parameterHint.typeName should be("TestObject1")

            member1.childrenHints.foreach {
              case id: JsonValueHint if id.parameterHint.field == "id" =>
                id.parameterHint.typeName should be("Int")

              case id2: JsonValueHint if id2.parameterHint.field == "id2" =>
                id2.parameterHint.typeName should be("Double")

              case invalidId => fail("invalid Id:" + invalidId)
            }

          case member2: JsonObjectHint
              if member2.parameterHint.field == "member2" =>
            member2.parameterHint.typeName should be("TestObject1")
            member2.childrenHints.foreach {
              case tt: JsonValueHint if tt.parameterHint.field == "tt" =>
                tt.parameterHint.field should be("tt")
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
