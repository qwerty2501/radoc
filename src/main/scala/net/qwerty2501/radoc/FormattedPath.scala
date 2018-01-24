package net.qwerty2501.radoc

private class FormattedPath private (override val displayPath: String,
                                     override val actualPath: String)
    extends Path {

  def this(pathFormat: String, params: Any*) = {

    this(
      pathFormat, {
        val format = ":[^/]+".r
        if (format.findAllMatchIn(pathFormat).size != params.size) {
          throw new IllegalArgumentException("should match params count.")
        }
        params.foldLeft(pathFormat) { (path, param) =>
          path.replaceFirst(format.findFirstIn(path).getOrElse(""),
                            param.toString)
        }
      }
    )
  }

}

private object FormattedPath {
  def apply(pathFormat: String, params: Any*) =
    new FormattedPath(pathFormat, params: _*)
}
