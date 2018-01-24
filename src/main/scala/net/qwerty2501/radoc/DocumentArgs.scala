package net.qwerty2501.radoc

case class DocumentArgs(category: String,
                        group: String,
                        description: String,
                        messageName: String,
                        version: Version,
                        extendArgs: Map[String, String]) {
  def this(category: String,
           description: String,
           version: Version,
           extendArgs: Map[String, String]) =
    this(category, "", description, "", version, extendArgs)
}

object DocumentArgs {
  def apply(category: String,
            description: String,
            version: Version,
            extendArgs: Map[String, String]) =
    new DocumentArgs(category, description, version, extendArgs)
}
