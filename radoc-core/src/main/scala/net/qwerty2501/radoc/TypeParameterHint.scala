package net.qwerty2501.radoc

case class TypeParameterHint[T](typeName: String,
                                description: Text,
                                assert: ParameterAssert[T],
                                essentiality: Essentiality) {

  def this(typeName: String, description: Text, essentiality: Essentiality) =
    this(typeName, description, ParameterAssert.default[T], essentiality)

  def this(typeName: String, description: Text) =
    this(typeName, description, Essentiality.Mandatory)
  def this(typeName: String) = this(typeName, Text())
  def toParameterHint(field: String): ParameterHint[T] =
    ParameterHint(field, typeName, description, assert, essentiality)

}

object TypeParameterHint {
  def apply[T](typeName: String,
               description: Text,
               essentiality: Essentiality): TypeParameterHint[T] =
    new TypeParameterHint(typeName, description, essentiality)

  def apply[T](typeName: String, description: Text): TypeParameterHint[T] =
    new TypeParameterHint(typeName, description)

  def apply[T](typeName: String): TypeParameterHint[T] =
    new TypeParameterHint(typeName)
}
