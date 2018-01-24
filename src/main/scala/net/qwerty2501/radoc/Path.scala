package net.qwerty2501.radoc

trait Path {

  val displayPath: String
  val actualPath: String

  override def hashCode() = (displayPath, actualPath).hashCode()
  override def equals(o: Any) = {
    o match {
      case path: Path => path.hashCode() == this.hashCode()
      case _          => false
    }

  }

  override def toString = displayPath
}

object Path {
  def apply(path: String): Path = AbsolutePath(path)
  def format(format: String, params: Any*): Path =
    FormattedPath(format, params: _*)
}
