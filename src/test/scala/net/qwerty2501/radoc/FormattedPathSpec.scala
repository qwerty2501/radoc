package net.qwerty2501.radoc

import org.scalatest._

class FormattedPathSpec extends FlatSpec with Matchers {

  it should "format params" in {
    val format = "/test/:id/path/:w2"
    val path = FormattedPath(format, 33, "tt")
    path.actualPath should be("/test/33/path/tt")
    path.displayPath should be(format)
  }

  "unmatched size params" should "throw IllegalArgumentException" in {
    val format = "/test/:d3"
    intercept[IllegalArgumentException] {
      FormattedPath(format, 33, "tt")
    }
  }

}
