package net.qwerty2501.radoc

case class RootAPIDocumentWithVersion(
    version: Version,
    apiCategories: Map[String, APIDocumentCategory])
