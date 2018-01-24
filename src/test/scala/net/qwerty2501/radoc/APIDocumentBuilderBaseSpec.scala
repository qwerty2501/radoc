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

  it should "can not add description twice to same api document" in {
    val apiDocumentBuilder = new APIDocumentBuilderMock()
    val path = "test/path"
    val description = "description"

    intercept[IllegalStateException] {
      apiDocumentBuilder.request(Request.get(path), description)
      apiDocumentBuilder.request(Request.get(path), description)
    }
  }

  it should "can add same path request but difference method" in{
    val apiDocumentBuilder = new APIDocumentBuilderMock()
    val targetPath = "test/path"
    apiDocumentBuilder.request(Request.get(targetPath))
    apiDocumentBuilder.request(Request.post(targetPath,Content()))
    val apiDocumentGroup = apiDocumentBuilder.getRootAPIDocument.documents(Version()).apiCategories("").apiDocumentGroups(targetPath)

    val getAPIDocument = apiDocumentGroup.apiDocuments(Method.Get,targetPath)
    getAPIDocument.messageDocuments.length should  be (1)
    val getMessageDocument = getAPIDocument.messageDocuments.head
    getMessageDocument.request.method should be (Method.Get)
    getMessageDocument.request.path.displayPath should be (targetPath)

    val postAPIDocument = apiDocumentGroup.apiDocuments(Method.Post,targetPath)
    postAPIDocument.messageDocuments.length should be (1)
    val postMessageDocument = postAPIDocument.messageDocuments.head
    postMessageDocument.request.method should be (Method.Post)
    postMessageDocument.request.path.displayPath should be (targetPath)

  }
}
