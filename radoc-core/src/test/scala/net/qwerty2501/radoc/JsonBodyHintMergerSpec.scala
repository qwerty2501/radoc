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
        JsonHint.Object(
          TypeParameterHint("TestObject", Text()),
          Map(
            "member1" -> JsonHint.Value(
              TypeParameterHint("Int", Text("member1_text"))),
            "member2" -> JsonHint.Value(
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

}
