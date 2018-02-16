package net.qwerty2501.radoc


import scala.tools.reflect.ToolBox
import com.github.dwickern.macros.NameOf._

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

object GenericJsonHintFactory {


  private case class FieldName(private val name: String) {
    def getName(fieldModifier: FieldModifier,
                fieldHintAnnotation: FieldHintAnnotation): String =
      if (fieldHintAnnotation.parameter.field != "")
        fieldHintAnnotation.parameter.field
      else fieldModifier.fieldModify(name)
  }
  private final val seqTypeName = classOf[Seq[_]].getName





  def generate[T: NotNothing](defaultFieldModifier:FieldModifier)= macro _generate[T]
  def _generate[T: c.TypeTag:NotNothing](c:blackbox.Context)(defaultFieldModifier:FieldModifier): JsonHint = {


    new DefaultGenerator(
      defaultFieldModifier,
      c).generate(
      FieldName(""),
      Option.empty,
      c.typeOf[T],
      FieldHintAnnotation.default)
  }

  def generateExpected[T:  NotNothing](
      expected: T,
      defaultFieldModifier: FieldModifier): JsonHint = ???

  private trait Generator {
    val defaultFieldModifier: FieldModifier
    val c:blackbox.Context
    val defaultAssertFactory: ParameterAssertFactory
    val mirror = c.mirror

    def generate(fieldName: FieldName,
                 value: Option[_],
                 t: Type,
                 fieldHintAnnotation: FieldHintAnnotation,
    ): JsonHint = {

      if (t.typeSymbol.fullName == seqTypeName) {
        generateArray(fieldName, value, t, fieldHintAnnotation)
      } else if (t.typeSymbol.asClass.isPrimitive || jsonValueTypes.exists(
                   jvt => t == jvt || t.baseClasses.exists(_ == jvt))) {
        generateValue(fieldName, value, t, fieldHintAnnotation)
      } else {
        generateObject(fieldName, value, t, fieldHintAnnotation)
      }

    }

    def generateArray(name: FieldName,
                      value: Option[_],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint
    def generateObject(name: FieldName,
                       value: Option[_],
                       t: Type,
                       fieldHintAnnotation: FieldHintAnnotation): JsonHint
    def generateValue(name: FieldName,
                      value: Option[_],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint

    def typeToClass(t: Type): Class[_] = {
      mirror.runtimeClass(t)
    }

    protected def getTypeName(t: Type): String =
      t.typeSymbol.asClass.name.toTypeName.toString

    def generateParameterHint(
        name: FieldName,
        value: Option[_],
        typeName: String,
        t: Type,
        fieldHintAnnotation: FieldHintAnnotation): ParameterHint =
      ParameterHint(
        Parameter(name.getName(defaultFieldModifier, fieldHintAnnotation),
                  value,
                  pickTypeName(typeName, fieldHintAnnotation),
                  fieldHintAnnotation.parameter.description),
        (if (fieldHintAnnotation.defaultParameterAssertFactory != ParameterAssertFactory.default)
           fieldHintAnnotation.defaultParameterAssertFactory
         else defaultAssertFactory)
          .generate(value, typeToClass(t)),
        fieldHintAnnotation.essentiality
      )

    def pickTypeName(typeName: String,
                     fieldHintAnnotation: FieldHintAnnotation): String =
      if (fieldHintAnnotation.parameter.typeName != "")
        fieldHintAnnotation.parameter.typeName
      else typeName
  }

  private class DefaultGenerator(
      override val defaultFieldModifier: FieldModifier,
      override val c:blackbox.Context)
      extends Generator {

    override val defaultAssertFactory: ParameterAssertFactory =
      ParameterAssertFactory.NoneAssertFactory

    def generateArray(name: FieldName,
                      value: Option[_],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint = {
      val typeArgName = t.typeArgs.headOption
        .fold("____unknown_generic_argument_type____")(getTypeName)

      JsonArrayHint(
        generateParameterHint(name,
                              value,
                              "[]" + typeArgName,
                              t,
                              fieldHintAnnotation),
        generate(FieldName(""),
                 Option.empty,
                 t.typeArgs.head,
                 FieldHintAnnotation.default),
        Seq()
      )
    }

    def generateObject(name: FieldName,
                       value: Option[_],
                       t: Type,
                       fieldHintAnnotation: FieldHintAnnotation): JsonHint = {

      JsonObjectHint(
        generateParameterHint(name,
                              value,
                              getTypeName(t),
                              t,
                              fieldHintAnnotation),
        t.members.collect {
          case m: MethodSymbol if m.isGetter && m.isPublic =>
            val faOption =
              m.annotations.find(_.tree.tpe =:= fieldAnnotationType)

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
                      value: Option[_],
                      t: Type,
                      fieldHintAnnotation: FieldHintAnnotation): JsonHint =
      JsonValueHint(
        generateParameterHint(name,
                              value,
                              getTypeName(t),
                              t,
                              fieldHintAnnotation))
  }

}
