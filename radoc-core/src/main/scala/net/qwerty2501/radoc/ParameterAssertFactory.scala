package net.qwerty2501.radoc

trait ParameterAssertFactory {
  def generate(expected: Option[_], expectedType: Class[_]): ParameterAssert
}

object ParameterAssertFactory {

  private[radoc] val default: ParameterAssertFactory = (_, _) =>
    ParameterAssert.default

  val NoneAssertFactory: ParameterAssertFactory = (_, _) => ParameterAssert()
  val EqualAssertFactory: ParameterAssertFactory = (expected, _) =>
    ParameterAssert.equalAssert(expected)

  def customEqualAssertFactory[T >: Any: NotNothing](
      eq: (T, T) => Boolean): ParameterAssertFactory =
    (expected, _) => ParameterAssert.equalAssert(expected, eq)

  val EqualTypeFactory: ParameterAssertFactory = (_, expectedType) =>
    ParameterAssert.typeEqualAssert(expectedType)

  def customEqualTypeFactory[A >: Any: NotNothing](
      eq: (Class[_], A) => Boolean): ParameterAssertFactory =
    (_, expectedType) => ParameterAssert.typeEqualAssert(expectedType, eq)

  def customAssert(assertHandler: ((Option[_], Parameter) => Unit)) =
    ParameterAssert(assertHandler)
}
