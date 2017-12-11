package net.qwerty2501.radoc

private case class RootAPIDocumentWithVersion(
    documents: Map[Version, RootAPIDocument]) {
  def this() = this(Map())
}
