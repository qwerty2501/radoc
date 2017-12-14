package net.qwerty2501.radoc

import net.qwerty2501.radoc.mocks._
import org.scalatest._

class APIDocumentBuilderBaseSpec extends FlatSpec with Matchers {

  it should "can add request response" in {
    val apiDocumentBuilder = new APIDocumentBuilderMock()
    val path = "test/path"
    val response = apiDocumentBuilder.request(Request.get(path))

    val apiDocument = apiDocumentBuilder.getRootAPIDocument
      .documents(Version())
      .apiCategories("")
      .apiDocumentGroups(path)
      .apiDocuments(Method.Get, path)
    apiDocument.description should be("")

    val messageDocument = apiDocument.messageDocuments.head

    messageDocument.request.path.displayPath should be(path)
    messageDocument.response should be(response)

  }

  it should "can add request response and description" in {
    val apiDocumentBuilder = new APIDocumentBuilderMock()
    val path = "test/path"
    val description = "description"
    val response = apiDocumentBuilder.request(Request.get(path), description)

    val apiDocument = apiDocumentBuilder.getRootAPIDocument
      .documents(Version())
      .apiCategories("")
      .apiDocumentGroups(path)
      .apiDocuments(Method.Get, path)
    apiDocument.description should be(description)

    val messageDocument = apiDocument.messageDocuments.head

    messageDocument.request.path.displayPath should be(path)
    messageDocument.response should be(response)
  }
}
