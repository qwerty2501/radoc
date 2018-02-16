package net.qwerty2501.radoc

import org.scalatest._
import net.qwerty2501.radoc._


case class TestChild(message: String, value: Double)

case class TestRoot(
    id: Int,
    child: TestChild,
    children: Seq[TestChild],
    hintTest: String,
    ids: Seq[Int],
    testAPIString: String,
    @FieldHintAnnotation(Text("test_document"))
    fieldAnnotationString: String,
    @FieldHintAnnotation(
      Parameter("f2", Option.empty, "Number", Text("test_document2")),
      Essentiality.OmitEmpty,
      ParameterAssertFactory.EqualTypeFactory)
    fieldAnnotationInt2: Int)

class GenericJsonHintFactorySpec extends FlatSpec with Matchers {

  it should "can construct JsonHint" in {

    val jsonHint =
      GenericJsonHintFactory.generate[TestRoot](FieldModifier.Snake)

    jsonHint match {
      case testRoot: JsonObjectHint =>
        testRoot.parameterHint.parameter.field should be("")
        testRoot.parameterHint.parameter.typeName should be("TestRoot")

        testRoot.parameterHint.essentiality should be(Essentiality.Mandatory)
        testRoot.parameterHint.assert should be(ParameterAssert.apply())
        testRoot.childrenHints.length should be(8)
        testRoot.childrenHints
          .map(_.parameterHint.parameter.field)
          .distinct
          .length should be(testRoot.childrenHints.length)
        testRoot.childrenHints.foreach {
          case idValue: JsonValueHint if idValue.parameterHint.field == "id" =>
            idValue.parameterHint.typeName should be("Int")

          case child: JsonObjectHint if child.parameterHint.field == "child" =>
            checkTestChildHint(child)

          case children: JsonArrayHint
              if children.parameterHint.field == "children" =>
            children.parameterHint.typeName should be("[]TestChild")
            children.childrenTypeHint match {
              case testChild: JsonObjectHint => checkTestChildHint(testChild)

              case invalidType =>
                fail("invalid type:" + invalidType.parameterHint.field)
            }

          case idHints: JsonArrayHint if idHints.parameterHint.field == "ids" =>
            idHints.parameterHint.typeName should be("[]Int")
            idHints.childrenTypeHint match {
              case idHint: JsonValueHint =>
                idHint.parameterHint.typeName should be("Int")
              case invalidType =>
                fail("invalid type:" + invalidType.parameterHint.typeName)
            }

          case apiString: JsonValueHint
              if apiString.parameterHint.field == "test_api_string" =>
            apiString.parameterHint.typeName should be("String")

          case hintTest: JsonValueHint
              if hintTest.parameterHint.field == "hint_test" =>
            hintTest.parameterHint.typeName should be("String")

          case fieldAnnotationString: JsonValueHint
              if fieldAnnotationString.parameterHint.field == "field_annotation_string" =>
            fieldAnnotationString.parameterHint.typeName should be("String")
            fieldAnnotationString.parameterHint.description should be(
              Text("test_document"))

          case fieldAnnotationInt2: JsonValueHint
              if fieldAnnotationInt2.parameterHint.field == "f2" =>
            fieldAnnotationInt2.parameterHint.typeName should be("Number")
            fieldAnnotationInt2.parameterHint.essentiality should be(
              Essentiality.OmitEmpty)
            fieldAnnotationInt2.parameterHint.assert should not be (ParameterAssert
              .apply())
          case invalidChild =>
            fail("invalid child:" + invalidChild.parameterHint.field)
        }

      case _ => fail("json hint is not object")
    }

  }

  private def checkTestChildHint(child: JsonObjectHint): Unit = {
    child.parameterHint.typeName should be("TestChild")
    child.childrenHints.length should be(2)
    child.childrenHints.map(_.parameterHint.field).distinct.length should be(
      child.childrenHints.length)
    child.childrenHints.foreach {
      case message: JsonValueHint if message.parameterHint.field == "message" =>
        message.parameterHint.typeName should be("String")

      case value: JsonValueHint if value.parameterHint.field == "value" =>
        value.parameterHint.typeName should be("Double")

      case invalidChild =>
        fail("invalid child:" + invalidChild.parameterHint.field)
    }
  }

}
