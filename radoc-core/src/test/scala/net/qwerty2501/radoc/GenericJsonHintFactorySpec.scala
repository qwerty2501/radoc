package net.qwerty2501.radoc

import org.scalatest._

class GenericJsonHintFactorySpec extends FlatSpec with Matchers {

  case class TestChild(message: String, value: Double)
  case class TestRoot(id: Int,
                      child: TestChild,
                      children: Seq[TestChild],
                      ids: Seq[Int])
  it should "can construct JsonHint" in {
    val jsonHint =
      GenericJsonHintFactory.generate[TestRoot](FieldModifier.Snake)

    jsonHint match {
      case testRoot: JsonObjectHint => {
        testRoot.parameterHint.parameter.field should be("")
        testRoot.parameterHint.parameter.typeName should be("TestRoot")

        testRoot.parameterHint.essentiality should be(Essentiality.Mandatory)
        testRoot.parameterHint.assert should be(ParameterAssert.apply())
        testRoot.childrenHints.length should be(4)
        testRoot.childrenHints
          .map(_.parameterHint.parameter.field)
          .distinct
          .length should be(testRoot.childrenHints.length)
        testRoot.childrenHints.foreach {
          case idValue: JsonValueHint
              if idValue.parameterHint.field == "id" => {}
          case child: JsonObjectHint
              if child.parameterHint.field == "child" => {}

          case children: JsonArrayHint
              if children.parameterHint.field == "children" => {}

          case ids: JsonArrayHint if ids.parameterHint.field == "ids" => {}

          case _ => fail("invalid child.")
        }
      }
      case _ => fail("json hint is not object")
    }
  }
}
