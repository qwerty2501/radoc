package net.qwerty2501.radoc

private case class APIDocumentGroup(
    group: String,
    apiDocuments: Map[(Method, Path), APIDocument])
