package net.qwerty2501.radoc

trait URLPath {

  val displayPath: String
  val actualPath: String
  val pathParameters: Seq[Parameter]
  val queries: Seq[Parameter]

  override def toString: String = displayPath
}

case class PathOfURLPath(override val displayPath: String,
                         override val actualPath: String,
                         override val pathParameters: Seq[Parameter])
    extends URLPath {
  override val queries: Seq[Parameter] = Nil
  def /(path: String): PathOfURLPath =
    PathOfURLPath(this.displayPath + "/" + path,
                  this.actualPath + "/" + path,
                  this.pathParameters)
  def /(parameterPath: Parameter): PathOfURLPath =
    PathOfURLPath(this.displayPath + "/:" + parameterPath.name,
                  this.actualPath + "/" + parameterPath.value,
                  this.pathParameters :+ parameterPath)

  def :?(queryParameter: Parameter): QueriesOfURLPath =
    QueriesOfURLPath(this.displayPath,
                     this.actualPath,
                     this.pathParameters,
                     this.queries :+ queryParameter)
}

case class QueriesOfURLPath(override val displayPath: String,
                            override val actualPath: String,
                            override val pathParameters: Seq[Parameter],
                            override val queries: Seq[Parameter])
    extends URLPath {
  def &(queryParameter: Parameter): QueriesOfURLPath =
    QueriesOfURLPath(this.displayPath,
                     this.actualPath,
                     this.pathParameters,
                     this.queries :+ queryParameter)
}

object URLPath {

  def apply(path: String): PathOfURLPath = PathOfURLPath(path, path, Nil)
  def /(path: String): PathOfURLPath = PathOfURLPath("", "", Nil) / path

  def /(pathParameter: Parameter): PathOfURLPath =
    PathOfURLPath("", "", Nil) / pathParameter
}
