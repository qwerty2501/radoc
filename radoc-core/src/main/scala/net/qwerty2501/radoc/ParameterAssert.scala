package net.qwerty2501.radoc

import java.util.Objects

import scala.reflect.ClassTag

trait ParameterAssert {
  def assert(actual: Option[_], parameter: Parameter)
}

object ParameterAssert {
  private[radoc] val default: ParameterAssert = (_, _) => ()
  private final val none: ParameterAssert = (_, _) => ()
  def apply(): ParameterAssert = none
  def equalAssert(expected: Option[_]): ParameterAssert =
    equalAssert(expected, Objects.equals)
  def equalAssert[T >: Any: NotNothing](
      expected: Option[_],
      eq: (T, T) => Boolean): ParameterAssert =
    (actual, _) =>
      if (!actual.fold(expected.isEmpty)(
            ac => expected.fold(false)(ex => eq(ex, ac))))
        throw new AssertionError(
          "The expected is" + expected.getOrNull.toString + "but the actual is" + actual.getOrNull.toString)

  def typeEqualAssert[T: NotNothing]()(
      implicit ctg: ClassTag[T]): ParameterAssert =
    typeEqualAssert(ctg.runtimeClass)

  def typeEqualAssert[T: NotNothing, A >: Any: NotNothing]()(
      implicit ctg: ClassTag[T],
      eq: (Class[_], A) => Boolean): ParameterAssert =
    typeEqualAssert(ctg.runtimeClass, eq)

  def typeEqualAssert(expected: Class[_]): ParameterAssert =
    typeEqualAssert(expected, (expected, actual) => actual.getClass == expected)
  def typeEqualAssert[A >: Any: NotNothing](
      expected: Class[_],
      eq: (Class[_], A) => Boolean): ParameterAssert =
    (actual, _) =>
      if (!actual.fold(expected == classOf[Nothing])(ac => eq(expected, ac)))
        throw new AssertionError(
          "The expected type is " + expected.getName + "but the actual is not equal " + actual.getClass.getName
      )

  def apply(assertHandler: ((Option[_], Parameter) => Unit)): ParameterAssert =
    (actual, parameter) => assertHandler(actual, parameter)
}
