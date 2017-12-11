package net.qwerty2501.radoc

import org.scalatest._

class APIDocumentSpec extends FlatSpec with Matchers {

  "unmatched path documents" should "throw IllegalArgumentException" in {
    intercept[IllegalArgumentException] {
      APIDocument(
        Seq(MessageDocument(Request.get("test/path1"), Response(Status.Ok)),
            MessageDocument(Request.get("test/path2"), Response(Status.Ok))),
        "description",
        Map()
      )
    }
  }

  "unmatched method documents" should "throw IllegalArgumentException" in {
    intercept[IllegalArgumentException] {
      APIDocument(
        Seq(MessageDocument(Request.post("test/path", Content()),
                            Response(Status.Ok)),
            MessageDocument(Request.get("test/path"), Response(Status.Ok))),
        "description",
        Map()
      )
    }
  }
}
