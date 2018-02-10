package net.qwerty2501.radoc
import java.util.InvalidPropertiesFormatException

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import com.github.dwickern.macros.NameOf._

object GenericJsonHintFactory {

  private type JsonFieldHandler =
    (FieldName, Option[Any], Type, FieldModifier) => JsonHint
  private case class FieldName(private val name: String) {
    def getName(fieldModifier: FieldModifier,
                fieldHintAnnotation: FieldHintAnnotation): String =
      if (fieldHintAnnotation.parameter.field != "")
        fieldHintAnnotation.parameter.field
      else fieldModifier.fieldModify(name)
  }
  private final val seqTypeName = classOf[Seq[_]].getName
  private val mirror = runtimeMirror(
    classOf[FieldHintAnnotation].getClassLoader)
  private val toolbox = mirror.mkToolBox()

  def generate[T: TypeTag](defaultFieldModifier: FieldModifier): JsonHint = {
    new DefaultGenerator(defaultFieldModifier).generate(
      FieldName(""),
      Option.empty,
      typeOf[T],
      FieldHintAnnotation.default)
  }

  def generateExpected[T: TypeTag](
      expected: T,
      defaultFieldModifier: FieldModifier): JsonHint = ???

  private trait Generator {
    val defaultFieldModifier: FieldModifier
    val defaultAssertFactory: ParameterAssertFactory

    def generate(fieldName: FieldName,
                 value: Option[Any],
                 t: Type,
                 fieldHintAnnotation: FieldHintAnnotation,
    ): JsonHint = {

      if (t.typeSymbol.fullName == seqTypeName) {
        generateArray(fieldName, value, t, fieldHintAnnotation)
      } else if (t.typeSymbol.asClass.isPrimitive) {
        generateValue(fieldName, value, t, fieldHintAnnotation)
      } else {
        generateValue(fieldName, value, t, fieldHintAnnotation)
      }

    }

    def generateArray(name: FieldName,
                      value: Option[Any],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint
    def generateObject(name: FieldName,
                       value: Option[Any],
                       t: Type,
                       fieldHintAnnotation: FieldHintAnnotation): JsonHint
    def generateValue(name: FieldName,
                      value: Option[Any],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint

    def typeToClass(t: Type): Class[_] = rootMirror.runtimeClass(t)

    def generateParameterHint(
        name: FieldName,
        value: Option[Any],
        typeName: String,
        t: Type,
        fieldHintAnnotation: FieldHintAnnotation): ParameterHint =
      ParameterHint(
        Parameter(name.getName(defaultFieldModifier, fieldHintAnnotation),
                  value.getOrElse(""),
                  typeName,
                  fieldHintAnnotation.parameter.description),
        (if (fieldHintAnnotation.defaultParameterAssertFactory != ParameterAssertFactory.default)
           fieldHintAnnotation.defaultParameterAssertFactory
         else defaultAssertFactory)
          .generate(value.getOrElse(""), typeToClass(t)),
        fieldHintAnnotation.essentiality
      )

    def pickTypeName(typeName: String,
                     fieldHintAnnotation: FieldHintAnnotation): String =
      if (fieldHintAnnotation.parameter.typeName != "")
        fieldHintAnnotation.parameter.typeName
      else typeName
  }

  private class DefaultGenerator(
      override val defaultFieldModifier: FieldModifier)
      extends Generator {

    override val defaultAssertFactory: ParameterAssertFactory =
      ParameterAssertFactory.NoneAssertFactory

    def generateArray(name: FieldName,
                      value: Option[Any],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint = {
      val typeArgName = t.typeArgs.headOption
        .fold("____unknown_generic_argument_type____")(_.toString)
      val typeName = pickTypeName("[]" + typeArgName, fieldHintAnnotation)

      JsonArrayHint(
        generateParameterHint(name, value, typeName, t, fieldHintAnnotation),
        if (typeName.replace("[]", "") != typeArgName) Seq()
        else
          Seq(
            generate(FieldName(""),
                     Option.empty,
                     t.typeArgs.head,
                     FieldHintAnnotation.default))
      )
    }

    def generateObject(name: FieldName,
                       value: Option[Any],
                       t: Type,
                       fieldHintAnnotation: FieldHintAnnotation): JsonHint = {

      JsonObjectHint(
        generateParameterHint(name,
                              value,
                              t.typeSymbol.name.toString,
                              t,
                              fieldHintAnnotation),
        t.members.collect {
          case m: MethodSymbol if m.isGetter && m.isPublic =>
            val faOption =
              m.annotations.find(_.tree.tpe =:= typeOf[FieldHintAnnotation])
            val fannotation = faOption.fold(FieldHintAnnotation.default) { p =>
              toolbox
                .eval(toolbox.untypecheck(p.tree))
                .asInstanceOf[FieldHintAnnotation]
            }

            if (fannotation.defaultParameterAssertFactory == ParameterAssertFactory.EqualAssertFactory) {
              throw new IllegalArgumentException(
                "invalid specify assert type. if you want assert equal value, use " + nameOf(
                  JsonBodyHint) + "." + nameOf(JsonBodyHint.expectedHint()))
            }

            generate(FieldName(m.name.toString),
                     Option.empty,
                     m.returnType,
                     fannotation)

        }.toSeq
      )
    }

    def generateValue(name: FieldName,
                      value: Option[Any],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint =
      JsonValueHint(
        generateParameterHint(
          name,
          value,
          pickTypeName(t.typeSymbol.name.toString, fieldHintAnnotation),
          t,
          fieldHintAnnotation))
  }

}
