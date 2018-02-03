package net.qwerty2501.radoc

trait UrlPath {

  private[radoc] val parts: Seq[PartOfUrlPath]

  override def toString: String = displayPath

  val displayPath: String =
    root + parts
      .collect {
        case plain: PlainPath             => plain.path
        case pathParameter: PathParameter => pathParameter.displayField
      }
      .mkString("/")

  val actualPath: String = {
    val qps = queryParameterParts
    root + parts
      .collect {
        case plain: PlainPath => plain.path
        case pathParameter: PathParameter =>
          pathParameter.parameter.value.toString
      }
      .mkString("/") + (if (qps.nonEmpty)
                          "?" + qps
                            .map { qp =>
                              qp.display
                            }
                            .mkString("&")
                        else "")
  }

  val pathParameters: Seq[Parameter] =
    pathParameterParts.map(_.parameter)

  val queries: Seq[Parameter] = queryParameterParts.map(_.parameter)

  private[radoc] def paths: Seq[PartOfUrlPath] = parts.collect {
    case plain: PlainPath             => plain
    case pathParameter: PathParameter => pathParameter
  }

  private[radoc] def pathParameterParts: Seq[PathParameter] = parts.collect {
    case pathParameter: PathParameter => pathParameter
  }

  private[radoc] def queryParameterParts: Seq[QueryParameter] = parts.collect {
    case queryParameter: QueryParameter => queryParameter
  }

  private[radoc] def root: String = {
    parts
      .collectFirst {
        case root: RootPath => root.path
      }
      .getOrElse("")

  }

}

private[radoc] trait PartOfUrlPath

private[radoc] case class RootPath() extends PartOfUrlPath {
  def path = "/"
}

private[radoc] case class PlainPath(path: String) extends PartOfUrlPath

private[radoc] case class PathParameter(parameter: Parameter)
    extends PartOfUrlPath {
  def displayField: String = "{" + parameter.field + "}"
}
private[radoc] case class QueryParameter(parameter: Parameter)
    extends PartOfUrlPath {
  def display: String = parameter.field + "=" + parameter.value
}

class PathOfUrlPath private[radoc] (override val parts: Seq[PartOfUrlPath])
    extends UrlPath {

  def /(path: String): PathOfUrlPath =
    new PathOfUrlPath(parts :+ PlainPath(path))
  def /(parameterPath: Parameter): PathOfUrlPath =
    new PathOfUrlPath(parts :+ PathParameter(parameterPath))

  def :?(queryParameter: Parameter): QueriesOfUrlPath =
    new QueriesOfUrlPath(parts :+ QueryParameter(queryParameter))
}

class QueriesOfUrlPath private[radoc] (override val parts: Seq[PartOfUrlPath])
    extends UrlPath {
  def &(queryParameter: Parameter): QueriesOfUrlPath =
    new QueriesOfUrlPath(parts :+ QueryParameter(queryParameter))
}

object UrlPath {

  def apply(path: String): PathOfUrlPath = {

    val targetPath = if (path.length > 0) {
      if (path.headOption.getOrElse("") == "/") {
        path.substring(1)
      } else {
        path
      }
    } else path
    new PathOfUrlPath(Seq(RootPath(), PlainPath(targetPath)))
  }

  def /(path: String): PathOfUrlPath =
    new PathOfUrlPath(Seq(RootPath(), PlainPath(path)))

  def /(pathParameter: Parameter): PathOfUrlPath =
    new PathOfUrlPath(Seq(RootPath(), PathParameter(pathParameter)))
}
