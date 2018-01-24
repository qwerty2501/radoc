package net.qwerty2501.radoc

case class APIDocumentCategory(
    category: String,
    apiDocumentGroups: Map[String, APIDocumentGroup]) {}
