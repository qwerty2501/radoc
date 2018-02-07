package net.qwerty2501.radoc

import scala.reflect._

case class ParameterHint(parameter: Parameter,
                         assert: ParameterAssert,
                         essentiality: Essentiality) {

  def this(field: String,
           valueType: Class[_],
           description: Text,
           essentiality: Essentiality) =
    this(Parameter(field, "", valueType, description),
         ParameterAssert(),
         essentiality)
  def this(field: String, valueType: Class[_], description: Text) =
    this(field, valueType, description, Essentiality.mandatory)

  def this(field: String,
           valueTypeName: String,
           description: Text,
           essentiality: Essentiality) =
    this(Parameter(field, "", valueTypeName, description),
         ParameterAssert(),
         essentiality)

  def this(field: String, valueTypeName: String, description: Text) =
    this(field, valueTypeName, description, Essentiality.mandatory)
  def this(parameter: Parameter, essentiality: Essentiality) =
    this(parameter, ParameterAssert(), essentiality)

  def this(parameter: Parameter) =
    this(parameter, ParameterAssert(), Essentiality.mandatory)
  def this(field: String,
           value: Any,
           description: Text,
           assert: ParameterAssert,
           essentiality: Essentiality) =
    this(Parameter(field, value, description), assert, essentiality)

  def this(field: String,
           value: Any,
           description: Text,
           assert: ParameterAssert) =
    this(Parameter(field, value, description), assert, Essentiality.mandatory)
}

object ParameterHint {

  def apply[T: ClassTag](
      field: String,
      description: Text,
      essentiality: Essentiality)(implicit ct: ClassTag[T]): ParameterHint =
    new ParameterHint(field, ct.runtimeClass, description, essentiality)

  def apply[T: ClassTag](field: String, description: Text)(
      implicit ct: ClassTag[T]): ParameterHint =
    new ParameterHint(field,
                      ct.runtimeClass,
                      description,
                      Essentiality.mandatory)

  def apply(field: String,
            valueTypeName: String,
            description: Text,
            essentiality: Essentiality): ParameterHint =
    new ParameterHint(field, valueTypeName, description, essentiality)

  def apply(field: String,
            valueTypeName: String,
            description: Text): ParameterHint =
    new ParameterHint(field, valueTypeName, description, Essentiality.mandatory)

  def apply(parameter: Parameter, essentiality: Essentiality): ParameterHint =
    new ParameterHint(parameter, essentiality)

  def apply(parameter: Parameter) =
    new ParameterHint(parameter, Essentiality.mandatory)

  def apply(field: String,
            value: Any,
            description: Text,
            assert: ParameterAssert,
            essentiality: Essentiality): ParameterHint =
    new ParameterHint(Parameter(field, value, description),
                      assert,
                      essentiality)
  def apply(field: String,
            value: Any,
            description: Text,
            assert: ParameterAssert): ParameterHint =
    new ParameterHint(Parameter(field, value, description),
                      assert,
                      Essentiality.mandatory)

  def withEqualAssert(field: String,
                      expected: Any,
                      description: Text,
                      essentiality: Essentiality): ParameterHint =
    ParameterHint(
      field,
      expected,
      description,
      ParameterAssert.equalAssert(expected),
      essentiality
    )

  def withEqualAssert(field: String,
                      expected: Any,
                      description: Text): ParameterHint =
    withEqualAssert(field, expected, description, Essentiality.mandatory)

  def withTypeAssert[T: ClassTag](
      field: String,
      description: Text,
      essentiality: Essentiality)(implicit ct: ClassTag[T]): ParameterHint = {
    val valueType = ct.runtimeClass
    ParameterHint(
      Parameter(field, "", valueType.getSimpleName, description),
      ParameterAssert.typeEqualAssert(valueType),
      essentiality
    )
  }
  def withTypeAssert[T: ClassTag](field: String,
                                  description: Text): ParameterHint =
    withTypeAssert[T](field, description, Essentiality.mandatory)

}
