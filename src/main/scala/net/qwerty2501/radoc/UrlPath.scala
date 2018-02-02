package net.qwerty2501.radoc

trait UrlPath {

  def displayPath: String
  def actualPath: String
  def pathParameters: Seq[Parameter]
  def queries: Seq[Parameter]

  override def toString: String = displayPath
}

private trait PartOfUrlPath

private case class PlainPath(path: String) extends PartOfUrlPath

private case class PathParameter(parameter: Parameter) extends PartOfUrlPath
private case class QueryParameter(parameter: Parameter) extends PartOfUrlPath

case class PathOfUrlPath(override val displayPath: String,
                         override val actualPath: String,
                         override val pathParameters: Seq[Parameter])
    extends UrlPath {
  override val queries: Seq[Parameter] = Nil
  def /(path: String): PathOfUrlPath =
    PathOfUrlPath(this.displayPath + "/" + path,
                  this.actualPath + "/" + path,
                  this.pathParameters)
  def /(parameterPath: Parameter): PathOfUrlPath =
    PathOfUrlPath(this.displayPath + "/:" + parameterPath.field,
                  this.actualPath + "/" + parameterPath.value,
                  this.pathParameters :+ parameterPath)

  def :?(queryParameter: Parameter): QueriesOfUrlPath =
    QueriesOfUrlPath(
      this.displayPath,
      this.actualPath + "?" + queryParameter.field + "=" + queryParameter.value.toString,
      this.pathParameters,
      this.queries :+ queryParameter)
}

case class QueriesOfUrlPath(override val displayPath: String,
                            override val actualPath: String,
                            override val pathParameters: Seq[Parameter],
                            override val queries: Seq[Parameter])
    extends UrlPath {
  def &(queryParameter: Parameter): QueriesOfUrlPath =
    QueriesOfUrlPath(
      this.displayPath,
      this.actualPath + "&" + queryParameter.field + "=" + queryParameter.value.toString,
      this.pathParameters,
      this.queries :+ queryParameter)
}

object UrlPath {

  def apply(path: String): PathOfUrlPath = PathOfUrlPath(path, path, Nil)
  def /(path: String): PathOfUrlPath = PathOfUrlPath("", "", Nil) / path

  def /(pathParameter: Parameter): PathOfUrlPath =
    PathOfUrlPath("", "", Nil) / pathParameter
}
