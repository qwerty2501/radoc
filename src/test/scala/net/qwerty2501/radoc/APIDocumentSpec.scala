package net.qwerty2501.radoc

import org.scalatest._

class APIDocumentSpec extends FlatSpec with Matchers {
  it should "can be create from request response pairs." in {
    val expectedPath = "/test/path"
    val expectedMethod = Method.Get
    val expectedStatus = Status.Ok
    val document = APIDocument.createFromRequestResponsePairs(
      Seq((Request.get(expectedPath) -> Response(expectedStatus))))

    document.apis.size should be(1)
    val requestResponseContainer = document.apis(expectedPath)
    requestResponseContainer.requestResponseDocuments.length should be(1)
    val requestResponses =
      requestResponseContainer.requestResponseDocuments.head.requestResponses
    requestResponses.length should be(1)
    val request = requestResponses.head._1
    val response = requestResponses.head._2
    request.path.actualPath should be(expectedPath)
    request.method should be(expectedMethod)
    response.status should be(expectedStatus)

  }

  it should "be categorized by path and grouped by method and path" in {
    val category1Path = "category1/path"
    val expectedCategory1Group1Pairs =
      Seq((Request.get(category1Path), Response(Status.Ok)),
          (Request.get(category1Path), Response(Status.Ok)))
    val expectedContent1 = "expectedContent1"
    val expectedContent2 = "expectedContent2"
    val expectedCategory1Group2Pairs =
      Seq(
        (Request.post(category1Path, expectedContent1), Response(Status.Ok)),
        (Request.post(category1Path, expectedContent2), Response(Status.Ok))
      )
    val category2PathFormat = "category2/:id"
    val expectedCategory2Group1Pairs =
      Seq((Request.get(FormattedPath(category2PathFormat, 32)),
           Response(Status.Ok)),
          (Request.get(FormattedPath(category2PathFormat, 22)),
           Response(Status.Ok)))

    val document = APIDocument.createFromRequestResponsePairs(
      expectedCategory1Group1Pairs ++ expectedCategory1Group2Pairs ++ expectedCategory2Group1Pairs)

    val category1 = document.apis(category1Path)
    category1.requestResponseDocuments.size should be(2)
    val category1Group1 = category1.requestResponseDocuments
      .filter(_.requestResponses.head._1.method == Method.Get)
      .head
    category1Group1.requestResponses
      .filter(_._1.method == Method.Get)
      .filter(_._1.path.actualPath == category1Path)
      .size should be(2)

    val category1Group2 = category1.requestResponseDocuments
      .filter(_.requestResponses.head._1.method == Method.Post)
      .head

    category1Group2.requestResponses
      .filter(_._1.method == Method.Post)
      .filter(_._1.path.actualPath == category1Path)
      .size should be(2)

    val category2 = document.apis(category2PathFormat)

    category2.requestResponseDocuments.size should be(1)

    category2.requestResponseDocuments.head.requestResponses
      .filter(_._1.method == Method.Get)
      .filter(_._1.path.displayPath == category2PathFormat)
      .size should be(2)
  }

}
