package net.qwerty2501.radoc

import org.scalatest._

class VersionSpec extends FlatSpec with Matchers {

  it should "be 0.0.0.0" in {
    Version().toString should be("0.0.0.0")
  }

  it should "be 3.2.4.5" in {
    Version(3, 2, 4, 5).toString should be("3.2.4.5")
  }

  "minus number" should "throw IllegalArgumentException" in {
    intercept[IllegalArgumentException] {
      Version(-1, -1, -1, -1)
    }
  }
}
