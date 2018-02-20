package net.qwerty2501.radoc

case class ParameterHint[T](field: String,
                            typeName: String,
                            description: Text,
                            assert: ParameterAssert[T],
                            essentiality: Essentiality) {

  def this(field: String,
           valueTypeName: String,
           description: Text,
           essentiality: Essentiality) =
    this(field, valueTypeName, description, ParameterAssert[T](), essentiality)

  def this(field: String, valueTypeName: String, description: Text) =
    this(field, valueTypeName, description, Essentiality.Mandatory)

  def toParameter: Parameter =
    Parameter(field, Option.empty, typeName, description)
}

object ParameterHint {

  def apply[T](field: String,
               valueTypeName: String,
               description: Text,
               essentiality: Essentiality): ParameterHint[T] =
    new ParameterHint(field, valueTypeName, description, essentiality)

  def apply[T](field: String,
               valueTypeName: String,
               description: Text): ParameterHint[T] =
    new ParameterHint(field, valueTypeName, description, Essentiality.Mandatory)

  def withEqualAssert[T](field: String,
                         expected: Option[T],
                         typeName: String,
                         description: Text,
                         essentiality: Essentiality): ParameterHint[T] =
    ParameterHint[T](field,
                     typeName,
                     description,
                     ParameterAssert.assertEqual(expected),
                     essentiality)

  def withEqualAssert[T](field: String,
                         expected: Option[T],
                         typeName: String,
                         description: Text): ParameterHint[T] =
    withEqualAssert[T](field,
                       expected,
                       typeName,
                       description,
                       Essentiality.Mandatory)

}
