package net.qwerty2501.radoc

case class ParameterHint(field: String,
                         typeName: String,
                         description: Text,
                         assert: ParameterAssert,
                         essentiality: Essentiality) {

  def this(field: String,
           valueTypeName: String,
           description: Text,
           essentiality: Essentiality) =
    this(field, valueTypeName, description, ParameterAssert(), essentiality)

  def this(field: String, valueTypeName: String, description: Text) =
    this(field, valueTypeName, description, Essentiality.Mandatory)



  def toParameter: Parameter =
    Parameter(field, Option.empty, typeName, description)
}

object ParameterHint {

  def apply(field: String,
            valueTypeName: String,
            description: Text,
            essentiality: Essentiality): ParameterHint =
    new ParameterHint(field, valueTypeName, description, essentiality)

  def apply(field: String,
            valueTypeName: String,
            description: Text): ParameterHint =
    new ParameterHint(field, valueTypeName, description, Essentiality.Mandatory)



  def withEqualAssert(field: String,
                      expected: Option[_],
                      typeName: String,
                      description: Text,
                      essentiality: Essentiality): ParameterHint =
    ParameterHint(field,
                  typeName,
                  description,
                  ParameterAssert.assertEqual(expected),
                  essentiality)

  def withEqualAssert(field: String,
                      expected: Option[Option[_]],
                      typeName: String,
                      description: Text): ParameterHint =
    withEqualAssert(field,
                    expected,
                    typeName,
                    description,
                    Essentiality.Mandatory)

}
