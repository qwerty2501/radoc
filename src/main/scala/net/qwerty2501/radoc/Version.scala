package net.qwerty2501.radoc

case class Version private (major: Int, minor: Int, build: Int, revision: Int) {
  if (major < 0 || minor < 0 || build < 0 || revision < 0) {
    throw new IllegalArgumentException()
  }

  override def toString = major + "." + minor + "." + build + "." + revision
}

object Version {
  def apply(): Version = Version(0)
  def apply(major: Int): Version = Version(major, 0)
  def apply(major: Int, minor: Int): Version = Version(major, minor, 0)
  def apply(major: Int, minor: Int, build: Int): Version =
    Version(major, minor, build, 0)

}
