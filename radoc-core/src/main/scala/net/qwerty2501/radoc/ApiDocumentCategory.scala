package net.qwerty2501.radoc

case class ApiDocumentCategory(
    category: String,
    apiDocumentGroups: Map[String, ApiDocumentGroup]) {}
