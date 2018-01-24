package net.qwerty2501.radoc

case class APIDocumentGroup(group: String,
                            apiDocuments: Map[(Method, String), APIDocument])
