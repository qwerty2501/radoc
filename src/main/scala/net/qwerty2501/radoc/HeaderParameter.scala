package net.qwerty2501.radoc

case class HeaderParameter(value: Any, typeName: String, description: Text) {}

object HeaderParameter {
  def apply(value: Any, description: Text): HeaderParameter =
    HeaderParameter(value, value.getClass, description)
  def apply[T](value: Any,
               valueType: Class[T],
               description: Text): HeaderParameter =
    HeaderParameter(value, valueType.getSimpleName, description)
}
