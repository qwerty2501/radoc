package net.qwerty2501.radoc

import net.qwerty2501.radoc.mocks._
import org.scalatest._
import net.qwerty2501.radoc._
class ApiDocumentBuilderSpec extends FlatSpec with Matchers {

  it should "can add request response" in {
    val apiDocumentBuilder = new ApiDocumentBuilderMock()
    val path = UrlPath / "test" / "path"
    val response = apiDocumentBuilder.request(Request.get(path))

    val apiDocument = apiDocumentBuilder.buildRootAPIDocument
      .documents(Version())
      .apiCategories("")
      .apiDocumentGroups(path.displayPath)
      .apiDocuments(Method.Get, path.displayPath)
    apiDocument.description should be(Text())

    val messageDocument = apiDocument.messageDocumentMap.head._2

    messageDocument.request.path.displayPath should be(path.displayPath)
    messageDocument.response should be(response)

  }

  it should "can add request response and description" in {
    val apiDocumentBuilder = new ApiDocumentBuilderMock()
    val path = UrlPath / "test/path"
    val description = Text("description")
    val response = apiDocumentBuilder.request(Request.get(path), description)

    val apiDocument = apiDocumentBuilder.buildRootAPIDocument
      .documents(Version())
      .apiCategories("")
      .apiDocumentGroups(path.displayPath)
      .apiDocuments(Method.Get, path.displayPath)
    apiDocument.description should be(description)

    val messageDocument = apiDocument.messageDocumentMap.head._2

    messageDocument.request.path.displayPath should be(path.displayPath)
    messageDocument.response should be(response)
  }

  it should "can not add description twice to same api document" in {
    val apiDocumentBuilder = new ApiDocumentBuilderMock()
    val path = UrlPath / "test/path"
    val description = Text("description")

    intercept[IllegalStateException] {
      apiDocumentBuilder.request(Request.get(path), description)
      apiDocumentBuilder.request(Request.get(path), description)
    }
  }

  it should "can add message document with same message name. But append with numbers." in {
    val apiDocumentBuilder = new ApiDocumentBuilderMock()
    val path = UrlPath / "test/path"

    apiDocumentBuilder.request(Request.get(path))
    apiDocumentBuilder.request(Request.get(path))

    val rootAPIDocument = apiDocumentBuilder.buildRootAPIDocument

    val apiDocument =
      rootAPIDocument.documents.values.head.apiCategories.values.head.apiDocumentGroups.values.head.apiDocuments.values.head

    apiDocument.messageDocumentMap.size should be(2)
    apiDocument.messageDocumentMap.keys.exists(_ == "[200 OK]") should be(true)
    apiDocument.messageDocumentMap.keys.exists(_ == "[200 OK]-2") should be(
      true)
  }

  it should "can add same path request but difference method" in {

    val apiDocumentBuilder = new ApiDocumentBuilderMock()
    val targetPath = UrlPath / "test/path"
    apiDocumentBuilder.request(Request.get(targetPath))
    apiDocumentBuilder.request(Request.post(targetPath, Content()))
    val apiDocumentGroup = apiDocumentBuilder.buildRootAPIDocument
      .documents(Version())
      .apiCategories("")
      .apiDocumentGroups(targetPath.displayPath)

    val getAPIDocument =
      apiDocumentGroup.apiDocuments(Method.Get, targetPath.displayPath)
    getAPIDocument.messageDocumentMap.values.size should be(1)
    val getMessageDocument = getAPIDocument.messageDocumentMap.head._2
    getMessageDocument.request.method should be(Method.Get)
    getMessageDocument.request.path.displayPath should be(
      targetPath.displayPath)

    val postAPIDocument =
      apiDocumentGroup.apiDocuments(Method.Post, targetPath.displayPath)
    postAPIDocument.messageDocumentMap.size should be(1)
    val postMessageDocument = postAPIDocument.messageDocumentMap.head._2
    postMessageDocument.request.method should be(Method.Post)
    postMessageDocument.request.path.displayPath should be(
      targetPath.displayPath)

  }
}
