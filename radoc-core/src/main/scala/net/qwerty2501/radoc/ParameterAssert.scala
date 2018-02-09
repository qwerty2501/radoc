package net.qwerty2501.radoc

import scala.reflect.ClassTag

trait ParameterAssert {
  def assert(actual: Any, parameterHint: ParameterHint)
}

private class ParameterEqualAssert(expected: Any) extends ParameterAssert {
  override def assert(actual: Any, parameterHint: ParameterHint): Unit =
    if (expected != actual)
      throw new AssertionError(
        "The expected is" + ParameterValue(expected).toString + "but the actual is" + ParameterValue(
          actual).toString)
}

private class ParamterTypeEqualAssert(expected: Class[_])
    extends ParameterAssert {
  override def assert(actual: Any, parameterHint: ParameterHint): Unit =
    if (actual != null && expected != actual.getClass)
      throw new AssertionError(
        "The expected type is " + expected.getName + "but the actual is not equal " + expected.getClass.getName
      )
}

private class ParameterOriginalAssert(
    assertHandler: ((Any, ParameterHint) => Unit))
    extends ParameterAssert {
  override def assert(actual: Any, parameterHint: ParameterHint): Unit =
    assertHandler(actual, parameterHint)
}

private class ParameterNothingAssert() extends ParameterAssert {
  override def assert(actual: Any, parameterHint: ParameterHint): Unit = {}
}

object ParameterAssert {
  private[radoc] val default = new ParameterNothingAssert {}
  private final val none = new ParameterNothingAssert()
  def apply(): ParameterAssert = none
  def equalAssert(expected: Any): ParameterAssert =
    new ParameterEqualAssert(expected)

  def typeEqualAssert[T]()(implicit ctg: ClassTag[T]): ParameterAssert =
    typeEqualAssert(ctg.runtimeClass)
  def typeEqualAssert(expected: Class[_]): ParameterAssert =
    new ParamterTypeEqualAssert(expected)

  def apply(assertHandler: ((Any, ParameterHint) => Unit)): ParameterAssert =
    new ParameterOriginalAssert(assertHandler)
}
