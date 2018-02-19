package net.qwerty2501.radoc

case class TypeParameterHint(typeName: String,
                              description: Text,
                              assert: ParameterAssert,
                              essentiality: Essentiality) {

  def this(typeName:String,description:Text, essentiality: Essentiality) = this(typeName,description,ParameterAssert.default,  essentiality)

  def this(typeName:String,description:Text) = this(typeName,description,Essentiality.Mandatory)
  def this(typeName:String) = this(typeName,Text())
  def  toParameterHint(field:String):ParameterHint = ParameterHint(field,typeName,description,assert,essentiality)

}

object TypeParameterHint {
  def apply(typeName:String,description:Text, essentiality: Essentiality) :TypeParameterHint = new TypeParameterHint(typeName,description,essentiality)

  def apply(typeName:String,description:Text) :TypeParameterHint = new TypeParameterHint(typeName,description)

  def apply(typeName:String):TypeParameterHint = new TypeParameterHint(typeName)
}
