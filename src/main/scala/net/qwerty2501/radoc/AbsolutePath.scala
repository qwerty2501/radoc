package net.qwerty2501.radoc

class AbsolutePath private (override val actualPath: String,
                            override val displayPath: String)
    extends Path {
  def this(path: String) = this(path, path)

}

object AbsolutePath {
  def apply(path: String) = new AbsolutePath(path)
}
