package net.qwerty2501.radoc
import scala.reflect.runtime.universe._

case class ParameterHint(parameter: Parameter,
                         assertFunc: (Any => Unit),
                         essentiality: Essentiality) {

  def this(field: String,
           valueType: Type,
           description: Text,
           essentiality: Essentiality) =
    this(Parameter(field, "", valueType, description), _ => {}, essentiality)
  def this(field: String, valueType: Type, description: Text) =
    this(field, valueType, description, Essentiality.mandatory)

  def this(field: String,
           valueTypeName: String,
           description: Text,
           essentiality: Essentiality) =
    this(Parameter(field, "", valueTypeName, description),
         _ => {},
         essentiality)

  def this(field: String, valueTypeName: String, description: Text) =
    this(field, valueTypeName, description, Essentiality.mandatory)
  def this(parameter: Parameter, essentiality: Essentiality) =
    this(parameter, _ => {}, essentiality)

  def this(parameter: Parameter) =
    this(parameter, _ => {}, Essentiality.mandatory)
  def this(field: String,
           value: Any,
           description: Text,
           assertFunc: (Any => Unit),
           essentiality: Essentiality) =
    this(Parameter(field, value, description), assertFunc, essentiality)

  def this(field: String,
           value: Any,
           description: Text,
           assertFunc: (Any => Unit)) =
    this(Parameter(field, value, description),
         assertFunc,
         Essentiality.mandatory)
}

object ParameterHint {

  def apply[T](field: String, description: Text, essentiality: Essentiality)(
      implicit tte: TypeTag[T]): ParameterHint =
    new ParameterHint(field, typeOf[T], description, essentiality)

  def apply[T](field: String, description: Text)(
      implicit tte: TypeTag[T]): ParameterHint =
    new ParameterHint(field, typeOf[T], description, Essentiality.mandatory)

  def apply(field: String,
            valueTypeName: String,
            description: Text,
            essentiality: Essentiality): ParameterHint =
    new ParameterHint(field, valueTypeName, description, essentiality)

  def apply(field: String,
            valueTypeName: String,
            description: Text): ParameterHint =
    new ParameterHint(field, valueTypeName, description, Essentiality.mandatory)

  def apply(parameter: Parameter, essentiality: Essentiality): ParameterHint =
    new ParameterHint(parameter, essentiality)

  def apply(parameter: Parameter) =
    new ParameterHint(parameter, Essentiality.mandatory)

  def apply(field: String,
            value: Any,
            description: Text,
            assertFunc: (Any => Unit),
            essentiality: Essentiality): ParameterHint =
    new ParameterHint(Parameter(field, value, description),
                      assertFunc,
                      essentiality)
  def apply(field: String,
            value: Any,
            description: Text,
            assertFunc: (Any => Unit)): ParameterHint =
    new ParameterHint(Parameter(field, value, description),
                      assertFunc,
                      Essentiality.mandatory)

  def withEqualAssert(field: String,
                      expected: Any,
                      description: Text,
                      essentiality: Essentiality): ParameterHint =
    ParameterHint(
      field,
      expected,
      description,
      actual => {

        if (expected != actual)
          throw new AssertionError(
            "The expected is" + ParameterValue(expected).toString + "but the actual is" + ParameterValue(
              actual).toString)
      },
      essentiality
    )

  def withEqualAssert(field: String,
                      expected: Any,
                      description: Text): ParameterHint =
    withEqualAssert(field, expected, description, Essentiality.mandatory)

  def withTypeAssert[T](
      field: String,
      description: Text,
      essentiality: Essentiality)(implicit tte: TypeTag[T]): ParameterHint = {
    val valueType = typeOf[T]
    ParameterHint(
      Parameter(field, "", valueType.typeSymbol.name.toString, description),
      actualValue => {

        if (actualValue != null && valueType.getClass != actualValue.getClass)
          throw new AssertionError(
            "The expected type is " + valueType.typeSymbol.fullName + "but the actual is not equal " + actualValue.getClass.getName
          )
        else if (essentiality == Essentiality.mandatory && actualValue == null)
          throw new AssertionError("The actual value is null")
      },
      essentiality
    )
  }
  def withTypeAssert[T](field: String, description: Text)(
      implicit tte: TypeTag[T]): ParameterHint =
    withTypeAssert[T](field, description, Essentiality.mandatory)

}
