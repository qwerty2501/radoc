package net.qwerty2501.radoc

case class Version(major: Int, minor: Int, build: Int, revision: Int) {
  if (major < 0 || minor < 0 || build < 0 || revision < 0) {
    throw new IllegalArgumentException()
  }

  def this(major: Int, minor: Int, build: Int) = this(major, minor, build, 0)
  def this(major: Int, minor: Int) = this(major, minor, 0)
  def this(major: Int) = this(major, 0)
  def this() = this(0)

  override def toString = mkString('.')

  def mkString(delimiter: Char): String =
    "" + major + delimiter + minor + delimiter + build + delimiter + revision
}

object Version {
  def apply(): Version = Version(0)
  def apply(major: Int): Version = Version(major, 0)
  def apply(major: Int, minor: Int): Version = Version(major, minor, 0)
  def apply(major: Int, minor: Int, build: Int): Version =
    new Version(major, minor, build)

}
