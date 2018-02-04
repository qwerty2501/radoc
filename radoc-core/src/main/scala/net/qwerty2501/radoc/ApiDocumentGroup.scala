package net.qwerty2501.radoc

case class ApiDocumentGroup(group: String,
                            apiDocuments: Map[(Method, String), ApiDocument],
                            category: String,
                            version: Version) {

  override def toString: String = group
}
