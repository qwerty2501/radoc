package net.qwerty2501.radoc

private class AbsolutePath private (override val actualPath: String,
                                    override val displayPath: String)
    extends Path {
  def this(path: String) = this(path, path)

}

private object AbsolutePath {
  def apply(path: String) = new AbsolutePath(path)
}
