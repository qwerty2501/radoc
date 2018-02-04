package net.qwerty2501.radoc

case class RootApiDocumentWithVersion(
    version: Version,
    apiCategories: Map[String, ApiDocumentCategory])
