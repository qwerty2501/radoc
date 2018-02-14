package net.qwerty2501.radoc

import org.scalatest._
import net.qwerty2501.radoc._
import scala.reflect.runtime.universe._
class GenericJsonHintFactorySpec extends FlatSpec with Matchers {

  case class TestChild(message: String, value: Double)
  case class TestRoot(id: Int,
                      child: TestChild,
                      children: Seq[TestChild],
                      hintTest: String,
                      ids: Seq[Int],
                      APIString: String)
  it should "can construct JsonHint" in {

    val jsonHint =
      GenericJsonHintFactory.generate[TestRoot](FieldModifier.Snake)

    jsonHint match {
      case testRoot: JsonObjectHint =>
        testRoot.parameterHint.parameter.field should be("")
        testRoot.parameterHint.parameter.typeName should be("TestRoot")

        testRoot.parameterHint.essentiality should be(Essentiality.Mandatory)
        testRoot.parameterHint.assert should be(ParameterAssert.apply())
        testRoot.childrenHints.length should be(6)
        testRoot.childrenHints
          .map(_.parameterHint.parameter.field)
          .distinct
          .length should be(testRoot.childrenHints.length)
        testRoot.childrenHints.foreach {
          case idValue: JsonValueHint if idValue.parameterHint.field == "id" =>
            idValue.parameterHint.typeName should be("Int")

          case child: JsonObjectHint if child.parameterHint.field == "child" =>
            child.parameterHint.typeName should be("TestChild")

          case children: JsonArrayHint
              if children.parameterHint.field == "children" =>
            children.parameterHint.typeName should be("[]TestChild")

          case idHints: JsonArrayHint if idHints.parameterHint.field == "ids" =>
            idHints.parameterHint.typeName should be("[]Int")

          case apiString: JsonValueHint
              if apiString.parameterHint.field == "api_string" =>
            apiString.parameterHint.typeName should be("String")

          case hintTest: JsonValueHint
              if hintTest.parameterHint.field == "hint_test" =>
            hintTest.parameterHint.typeName should be("String")

          case invalidChild =>
            fail("invalid child:" + invalidChild.parameterHint.field)
        }

      case _ => fail("json hint is not object")
    }
  }
}
