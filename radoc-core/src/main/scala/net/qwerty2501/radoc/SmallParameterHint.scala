package net.qwerty2501.radoc

case class SmallParameterHint[T](
    description: Text,
    assert: ParameterAssert[T],
    essentiality: Essentiality
) {
  def this(description: Text, assert: ParameterAssert[T]) =
    this(description, assert, Essentiality.Mandatory)
  def this(description: Text) =
    this(description, ParameterAssert[T]())

  def toTypeParameterHint(typeName: String): TypeParameterHint[T] =
    TypeParameterHint(typeName, description, assert, essentiality)
}

object SmallParameterHint {
  def apply[T](description: Text,
               assert: ParameterAssert[T]): SmallParameterHint[T] =
    new SmallParameterHint(description, assert)

  def apply[T](description: Text): SmallParameterHint[T] =
    new SmallParameterHint(description)

  private[radoc] def apply[T](): SmallParameterHint[T] =
    new SmallParameterHint[T](Text())

}
