package net.qwerty2501.radoc

case class DocumentArgs(category: String,
                        group: String,
                        description: Text,
                        messageName: String,
                        version: Version) {
  def this(category: String, description: Text, version: Version) =
    this(category, "", description, "", version)
}

object DocumentArgs {
  def apply(category: String, description: Text, version: Version) =
    new DocumentArgs(category, description, version)
}
