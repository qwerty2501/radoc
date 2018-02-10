package net.qwerty2501.radoc

import scala.reflect.ClassTag

trait ParameterAssert {
  def assert(actual: Any, parameter: Parameter)
}

object ParameterAssert {
  private[radoc] val default: ParameterAssert = (_, _) => ()
  private final val none: ParameterAssert = (_, _) => ()
  def apply(): ParameterAssert = none
  def equalAssert(expected: Any): ParameterAssert =
    (actual, _) =>
      if (expected != actual)
        throw new AssertionError(
          "The expected is" + ParameterValue(expected).toString + "but the actual is" + ParameterValue(
            actual).toString)

  def typeEqualAssert[T]()(implicit ctg: ClassTag[T]): ParameterAssert =
    typeEqualAssert(ctg.runtimeClass)

  def typeEqualAssert(expected: Class[_]): ParameterAssert =
    (actual, _) =>
      if (actual != null && expected != actual.getClass)
        throw new AssertionError(
          "The expected type is " + expected.getName + "but the actual is not equal " + actual.getClass.getName
      )

  def apply(assertHandler: ((Any, Parameter) => Unit)): ParameterAssert =
    (actual, parameter) => assertHandler(actual, parameter)
}
