package net.qwerty2501.radoc

import java.util.Objects

trait ParameterAssert {
  def assert(actual: Option[_])
}

object ParameterAssert {
  private[radoc] val default: ParameterAssert = (_) => ()
  private final val none: ParameterAssert = (_) => ()
  def apply(): ParameterAssert = none
  def assertEqual(expected: Option[_]): ParameterAssert =
    assertEqual(expected, Objects.equals)
  def assertEqual[T >: Any: NotNothing](
      expected: Option[_],
      eq: (T, T) => Boolean): ParameterAssert =
    (actual) =>
      if (!actual.fold(expected.isEmpty)(
            ac => expected.fold(false)(ex => eq(ex, ac))))
        throw new AssertionError(
          "The expected is" + expected.getOrNull.toString + "but the actual is" + actual.getOrNull.toString)

  def apply(
      assertHandler: ((Option[_]) => Unit)): ParameterAssert =
    (actual) => assertHandler(actual)
}
