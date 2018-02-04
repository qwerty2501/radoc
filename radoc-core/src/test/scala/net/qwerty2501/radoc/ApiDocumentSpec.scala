package net.qwerty2501.radoc

import org.scalatest._

class ApiDocumentSpec extends FlatSpec with Matchers {

  "unmatched path documents" should "throw IllegalArgumentException" in {
    intercept[IllegalArgumentException] {
      ApiDocument(
        Method.Get,
        UrlPath / "test/path1",
        Map(
          "v1" -> MessageDocument("v1",
                                  Request.get(UrlPath / "test/path1"),
                                  Response(Status.Ok)),
          "v2" -> MessageDocument("v2",
                                  Request.get(UrlPath / "test/path2"),
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
      ApiDocument(
        Method.Get,
        UrlPath / "test/path",
        Map(
          "v1" -> MessageDocument("v1",
                                  Request.post(UrlPath / "test/path",
                                               Content()),
                                  Response(Status.Ok)),
          "v2" -> MessageDocument("v2",
                                  Request.get(UrlPath / "test/path"),
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
