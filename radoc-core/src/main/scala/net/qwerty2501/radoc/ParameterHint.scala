package net.qwerty2501.radoc

case class ParameterHint(parameter: Parameter, assertFunc: (Any => Unit)) {
  def this(field: String, valueType: Class[_], description: Text) =
    this(Parameter(field, "", valueType, description), _ => {})
  def this(field: String, valueTypeName: String, description: Text) =
    this(Parameter(field, "", valueTypeName, description), _ => {})
  def this(parameter: Parameter) = this(parameter, _ => {})

  def this(field: String,
           value: Any,
           description: Text,
           assertFunc: (Any => Unit)) =
    this(Parameter(field, value, description), assertFunc)
}

object ParameterHint {

  def apply(field: String,
            valueType: Class[_],
            description: Text): ParameterHint =
    new ParameterHint(field, valueType, description)

  def apply(field: String,
            valueTypeName: String,
            description: Text): ParameterHint =
    new ParameterHint(field, valueTypeName, description)

  def apply(parameter: Parameter) = new ParameterHint(parameter)

  def apply(field: String,
            value: Any,
            description: Text,
            assertFunc: (Any => Unit)): ParameterHint =
    new ParameterHint(Parameter(field, value, description), assertFunc)

  def withEqualAssert(field: String, expected: Any, description: Text) =
    ParameterHint(
      field,
      expected,
      description,
      actual => {

        if (expected != actual)
          throw new AssertionError(
            "The expected is" + ParameterValue(expected).toString + "but the actual is" + ParameterValue(
              actual).toString)
      }
    )

  def withTypeAssert(field: String, expected: Class[_], description: Text) =
    ParameterHint(
      Parameter(field, "", expected.getSimpleName, description),
      actualValue => {

        if (actualValue != null && expected != actualValue.getClass)
          throw new AssertionError(
            "The expected type is " + expected.getName + "but the actual is not equal " + actualValue.getClass.getName
          )
        else if (actualValue == null)
          throw new AssertionError("The actual value is null")
      }
    )
}
