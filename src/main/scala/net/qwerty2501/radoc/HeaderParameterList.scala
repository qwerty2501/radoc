package net.qwerty2501.radoc

class HeaderParameterList(private var headers: Seq[Parameter]) {
  headers = headers
    .groupBy(_.field)
    .map { t =>
      Parameter(
        t._1,
        t._2
          .map(_.value)
          .mkString(","),
        t._2.map(_.typeName).mkString(","),
        t._2
          .map(_.description)
          .headOption
          .find(_ != Text())
          .getOrElse(Text())
      )
    }
    .toSeq

  def getHeaders: Seq[Parameter] = headers
}

object HeaderParameterList {
  def apply(headers: Seq[Parameter]): HeaderParameterList =
    new HeaderParameterList(headers)
}
