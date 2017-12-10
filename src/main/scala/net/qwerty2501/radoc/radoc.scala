package net.qwerty2501

package object radoc {
  implicit def stringToPath(str: String): Path = AbsolutePath(str)
}
