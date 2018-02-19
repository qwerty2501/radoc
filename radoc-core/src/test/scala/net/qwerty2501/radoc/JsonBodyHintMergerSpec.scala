package net.qwerty2501.radoc

import org.scalatest._
import io.circe._
import io.circe.parser._

class JsonBodyHintMergerSpec extends FlatSpec with Matchers {

  private def json(jsonString: String): Json =
    parse(jsonString).getOrElse(Json.Null)

  private def merge(text: String, jsonBodyHint: JsonBodyHint) =
    new JsonBodyHintMerger().merge(
      json(text),
      jsonBodyHint
    )

  private def checkParameter(parameter: Parameter,
                             field: String,
                             typeName: String,
                             description: Text) = {
    parameter.field should be(field)
    parameter.typeName should be(typeName)
    parameter.description should be(description)
  }

  it should "can parse" in {

    val newHint = merge(
      """
       |{
       | "member1":33,
       | "member2":"test member"
       |}
     """.stripMargin,
      JsonBodyHint()
    )

    newHint.typeParameterMap.size should be(1)

    val parameters = newHint.typeParameterMap("UnknownObject1")
    parameters.length should be(2)
    checkParameter(parameters.head, "member1", "Number", Text())

    checkParameter(parameters(1), "member2", "String", Text())

  }

  it should "can parse with JsonBodyHint" in {
    val newHint = merge(
      """
        |{
        | "member1":33,
        | "member2":"test member",
        | "member3":{
        |   "child_member1":"tte",
        |   "child_member2":33
        | }
        |}
      """.stripMargin,
      JsonBodyHint(
        JsonObjectHint(
          TypeParameterHint("TestObject", Text()),
          Map(
            "member1" -> JsonValueHint(
              TypeParameterHint("Int", Text("member1_text"))),
            "member2" -> JsonValueHint(
              TypeParameterHint("String", Text("member2_text")))
          )
        ))
    )

    newHint.typeParameterMap.size should be(2)

    val parameters = newHint.typeParameterMap("TestObject")
    parameters.length should be(3)
    checkParameter(parameters.head, "member1", "Int", Text("member1_text"))

    checkParameter(parameters(1), "member2", "String", Text("member2_text"))

    checkParameter(parameters(2), "member3", "UnknownObject1", Text())

    val childParameters = newHint.typeParameterMap("UnknownObject1")
    childParameters.length should be(2)

    checkParameter(childParameters.head, "child_member1", "String", Text())

    checkParameter(childParameters(1), "child_member2", "Number", Text())

  }

  it should "first element to type map " in {
    val newHint = merge(
      """
        |{
        | "test_array":[
        |   {
        |     "member1":333
        |   },
        |   {
        |     "member2":"test"
        |   }
        | ]
        |}
      """.stripMargin,
      JsonBodyHint()
    )

    newHint.typeParameterMap.size should be(2)

    val unknownObject1 = newHint.typeParameterMap("UnknownObject1")
    unknownObject1.length should be(1)
    unknownObject1.head.field should be("test_array")
    unknownObject1.head.typeName should be("[]UnknownObject2")

    val unknownObject2 = newHint.typeParameterMap("UnknownObject2")
    unknownObject2.length should be(1)
    unknownObject2.head.field should be("member1")
    unknownObject2.head.typeName should be("Number")
  }

}
