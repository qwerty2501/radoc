package net.qwerty2501.radoc

trait UrlPath {

  private[radoc] val parts: Seq[PartOfUrlPath]

  override def toString: String = displayPath

  val displayPath: String =
    parts
      .collect {
        case plain: PlainPath             => plain.path
        case pathParameter: PathParameter => pathParameter.displayField
        case p: Separator                 => p.toString
      }
      .mkString("")

  val actualPath: String = {
    val qps = queryParameterParts
    parts
      .collect {

        case plain: PlainPath => plain.path
        case pathParameter: PathParameter =>
          pathParameter.parameter.value.toString
        case queryParameter: QueryParameter => queryParameter.display
        case p                              => p.toString
      }
      .mkString("")
  }

  val pathParameters: Seq[Parameter] =
    pathParameterParts.map(_.parameter)

  val queries: Seq[Parameter] = queryParameterParts.map(_.parameter)

  private[radoc] def paths: Seq[PartOfUrlPath] = parts.collect {
    case root: Separator              => root
    case plain: PlainPath             => plain
    case pathParameter: PathParameter => pathParameter
  }

  private[radoc] def pathParameterParts: Seq[PathParameter] = parts.collect {
    case pathParameter: PathParameter => pathParameter
  }

  private[radoc] def queryParameterParts: Seq[QueryParameter] = parts.collect {
    case queryParameter: QueryParameter => queryParameter
  }

}

private[radoc] trait PartOfUrlPath

private[radoc] case class Separator() extends PartOfUrlPath {
  override def toString: String = "/"
}

private[radoc] case class Question() extends PartOfUrlPath {
  override def toString: String = "?"
}

private[radoc] case class Ampersand() extends PartOfUrlPath {
  override def toString: String = "&"
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
    new PathOfUrlPath(parts ++ Seq(Separator(), PlainPath(path)))
  def /(parameterPath: Parameter): PathOfUrlPath =
    new PathOfUrlPath(parts ++ Seq(Separator(), PathParameter(parameterPath)))

  def :?(queryParameter: Parameter): QueriesOfUrlPath =
    new QueriesOfUrlPath(
      parts ++ Seq(Question(), QueryParameter(queryParameter)))
}

class QueriesOfUrlPath private[radoc] (override val parts: Seq[PartOfUrlPath])
    extends UrlPath {
  def &(queryParameter: Parameter): QueriesOfUrlPath =
    new QueriesOfUrlPath(
      parts ++ Seq(Ampersand(), QueryParameter(queryParameter)))
}

object UrlPath {

  def apply(path: String): PathOfUrlPath = {

    val targetPath = if (path.nonEmpty) {
      if (path.headOption.getOrElse("") == "/") {
        path.substring(1)
      } else {
        path
      }
    } else path
    new PathOfUrlPath(Seq(Separator(), PlainPath(targetPath)))
  }

  def /(path: String): PathOfUrlPath =
    new PathOfUrlPath(Seq(Separator(), PlainPath(path)))

  def /(pathParameter: Parameter): PathOfUrlPath =
    new PathOfUrlPath(Seq(Separator(), PathParameter(pathParameter)))
}
