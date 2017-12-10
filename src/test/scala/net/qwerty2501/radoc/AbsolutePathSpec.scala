package net.qwerty2501.radoc

import org.scalatest._

class AbsolutePathSpec extends FlatSpec with Matchers {

  it should "path to actual path and display path" in {
    val expectedPath = "test/path"
    val path = AbsolutePath(expectedPath)
    path.displayPath should be(expectedPath)
    path.actualPath should be(expectedPath)
  }
}
