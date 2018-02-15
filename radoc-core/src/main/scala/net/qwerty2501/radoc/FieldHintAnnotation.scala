package net.qwerty2501.radoc

import scala.annotation.StaticAnnotation
import scala.annotation.meta.getter

@getter
case class FieldHintAnnotation(
    parameter: Parameter,
    essentiality: Essentiality,
    defaultParameterAssertFactory: ParameterAssertFactory)
    extends StaticAnnotation {

  def this(parameter: Parameter,
           defaultParameterAssertFactory: ParameterAssertFactory) =
    this(parameter, Essentiality.Mandatory, defaultParameterAssertFactory)

  def this(parameter: Parameter) =
    this(parameter, ParameterAssertFactory.default)

  def this(description: Text) = this(Parameter("", description))
}

private object FieldHintAnnotation {
  val default = new FieldHintAnnotation(Text())
}
