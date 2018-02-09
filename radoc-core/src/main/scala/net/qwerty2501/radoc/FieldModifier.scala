package net.qwerty2501.radoc

trait FieldModifier {
  private[radoc] def fieldModify(field: String): String
}

final object FieldModifier {
  final val None: FieldModifier = field => field

  final val Snake: FieldModifier = field =>
  "[A-Z]".r.replaceAllIn("[^A-Z]([A-Z)".r.replaceAllIn(field, { m =>
    "_" + m.group(1).toLowerCase
  }), { m =>
    m.group(0).toLowerCase
  })

  final val Camel: FieldModifier = field  =>
  "_([a-z\\d])".r.replaceAllIn(field, { m =>
    m.group(1).toUpperCase
  })
}
