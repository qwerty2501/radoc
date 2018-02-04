package net.qwerty2501.radoc

import net.qwerty2501.radoc

private final class ContentType private ()

private object ContentType {
  val Json = new ContentType()
  val Xml = new ContentType()
  val Text = new ContentType()
  private[radoc] def apply(headers: HeaderParameterList): ContentType = {
    val contentType =
      headers.getHeaders.headOption
        .find(_.field == "Content-Type")
        .getOrElse(Parameter("Content-Type", "", radoc.Text()))
        .value
        .toString

    if (contentType.contains("json")) {
      Json
    } else if (contentType.contains("xml")) {
      Xml
    } else {
      Text
    }
  }

}
