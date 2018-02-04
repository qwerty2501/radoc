package net.qwerty2501.radoc

case class RootApiDocument(title: String,
                           documents: Map[Version, RootApiDocumentWithVersion])
