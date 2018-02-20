package net.qwerty2501.radoc

import java.util.Objects

trait ParameterAssert[T] {
  def apply(actual: Option[T])
}

object ParameterAssert {
  private val default_ : ParameterAssert[_] = (_) => ()
  private[radoc] def default[T]: ParameterAssert[T] =
    default_.asInstanceOf[ParameterAssert[T]]
  private final val none_ : ParameterAssert[_] = (_) => ()
  def apply[T](): ParameterAssert[T] = none_.asInstanceOf[ParameterAssert[T]]
  def assertEqual[T](expected: Option[T]): ParameterAssert[T] =
    assertEqual[T](expected, Objects.equals(_, _))
  def assertEqual[T](expected: Option[T],
                     eq: (T, T) => Boolean): ParameterAssert[T] =
    (actual) =>
      if (!actual.fold(expected.isEmpty)(
            ac => expected.fold(false)(ex => eq(ex, ac))))
        throw new AssertionError(
          "The expected is" + expected.getOrNull.toString + "but the actual is" + actual.getOrNull.toString)

  def apply(assertHandler: ((Option[_]) => Unit)): ParameterAssert[_] =
    (actual) => assertHandler(actual)
}
