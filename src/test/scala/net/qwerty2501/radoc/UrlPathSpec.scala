package net.qwerty2501.radoc

import org.scalatest._

class UrlPathSpec extends FlatSpec with Matchers {

  it should "parameter path" in {
    val parameter = Parameter("id", 33, Text("id description"))
    val queryParameter =
      Parameter("name", "value", Text("name description"))
    val queryParameter2 =
      Parameter("name2", "value2", Text("name description"))
    val urlPath = UrlPath / "test/path" / parameter :? queryParameter & queryParameter2

    urlPath.displayPath should be("/test/path/{id}")
    urlPath.actualPath should be("/test/path/33?name=value&name2=value2")
    urlPath.pathParameters should be(Seq(parameter))

    urlPath.queries.length should be(2)
    urlPath.queries.head should be(queryParameter)
    urlPath.queries(1) should be(queryParameter2)
  }
}
