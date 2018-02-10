package net.qwerty2501.radoc

trait ParameterAssertFactory {
  def generate(expected: Any, expectedType: Class[_]): ParameterAssert
}

object ParameterAssertFactory {

  private[radoc] val default: ParameterAssertFactory = (_, _) =>
    ParameterAssert.default

  val NoneAssertFactory: ParameterAssertFactory = (_, _) => ParameterAssert()
  val EqualAssertFactory: ParameterAssertFactory = (expected, _) =>
    ParameterAssert.equalAssert(expected)

  val EqualTypeFactory: ParameterAssertFactory = (_, expectedType) =>
    ParameterAssert.typeEqualAssert(expectedType)

  def originalAssert(assertHandler: ((Any, Parameter) => Unit)) =
    ParameterAssert(assertHandler)
}
