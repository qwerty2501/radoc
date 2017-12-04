package net.qwerty2501.radoc

case class RequestResponseContainer(
    id: String,
    requestResponseDocuments: Seq[RequestResponseDocument]) {

  def this(requestResponseDocuments: Seq[RequestResponseDocument]) =
    this("", requestResponseDocuments)
}

object RequestResponseContainer {

  def apply(requestResponseDocuments: Seq[RequestResponseDocument])
    : RequestResponseContainer =
    new RequestResponseContainer(requestResponseDocuments)
}
