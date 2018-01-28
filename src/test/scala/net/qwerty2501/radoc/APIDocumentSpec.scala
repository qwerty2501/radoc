package net.qwerty2501.radoc

import org.scalatest._

class APIDocumentSpec extends FlatSpec with Matchers {

  "unmatched path documents" should "throw IllegalArgumentException" in {
    intercept[IllegalArgumentException] {
      APIDocument(
        Method.Get,
        URLPath / "test/path1",
        Map(
          "v1" -> MessageDocument("v1",
                                  Request.get(URLPath / "test/path1"),
                                  Response(Status.Ok)),
          "v2" -> MessageDocument("v2",
                                  Request.get(URLPath / "test/path2"),
                                  Response(Status.Ok))
        ),
        Text("description"),
        "group",
        "category",
        Version()
      )
    }
  }

  "unmatched method documents" should "throw IllegalArgumentException" in {
    intercept[IllegalArgumentException] {
      APIDocument(
        Method.Get,
        URLPath / "test/path",
        Map(
          "v1" -> MessageDocument("v1",
                                  Request.post(URLPath / "test/path",
                                               Content()),
                                  Response(Status.Ok)),
          "v2" -> MessageDocument("v2",
                                  Request.get(URLPath / "test/path"),
                                  Response(Status.Ok))
        ),
        Text("description"),
        "group",
        "category",
        Version()
      )
    }
  }
}
