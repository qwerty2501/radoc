package net.qwerty2501.radoc

private case class APIDocumentGroup(id: String,
                                    requestResponses: Seq[APIDocument],
                                    extendArgs: Map[String, String])
