package net.qwerty2501.radoc

trait FieldModifier {
  private[radoc] def fieldModify(field: String): String
}

private class NoneFieldModifier() extends FieldModifier {
  override private[radoc] def fieldModify(field: String): String = field
}

private class SnakeFieldModifier() extends FieldModifier {
  override def fieldModify(field: String): String =
    "[A-Z]".r.replaceAllIn("[^A-Z]([A-Z)".r.replaceAllIn(field, { m =>
      "_" + m.group(1).toLowerCase
    }), { m =>
      m.group(0).toLowerCase
    })
}

private class CamelFieldModifier() extends FieldModifier {
  override private[radoc] def fieldModify(field: String): String =
    "_([a-z\\d])".r.replaceAllIn(field, { m =>
      m.group(1).toUpperCase
    })
}

object FieldModifier {
  val none: FieldModifier = new NoneFieldModifier()
  val snake: FieldModifier = new SnakeFieldModifier()
  val camel: FieldModifier = new CamelFieldModifier()
}
