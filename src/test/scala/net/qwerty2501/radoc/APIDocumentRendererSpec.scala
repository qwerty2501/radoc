package net.qwerty2501.radoc

import java.io.File
import java.nio.file._

import net.qwerty2501.radoc.mocks.APIDocumentBuilderMock
import org.scalatest._

import scala.io.Source

class APIDocumentRendererSpec extends FlatSpec with Matchers {

  private def getSampleDocument = {
    val builder = new APIDocumentBuilderMock()
    val sampleJson = """
                       |{
                       | "member":1,
                       |}
                     """.stripMargin
    val sampleResponse = Response(
      Status.Ok,
      Map(
        ("Content-Type",
         HeaderParameter("application/json", Text("content type")))
      ),
      sampleJson
    )
    builder.append(
      Request.get(
        URLPath / "sample/path" / Parameter("id", 32, Text("id description"))
          :? Parameter("name", "john", Text("name description"))),
      sampleResponse
    )
    builder.append(
      Request.post(URLPath / "sample/path",
                   Map(
                     ("Content-Type",
                      HeaderParameter("application/json", Text("content type")))
                   ),
                   sampleJson),
      sampleResponse)
    builder.append(
      Request.get(
        URLPath / "sample/path2",
        Map(
          ("Accept-Language", HeaderParameter("en-US", Text("accept language")))
        )),
      Response(Status.Ok))
    builder.setRootDocumentTitle("sample title")
    builder.getRootAPIDocument
  }

  it should "can generate api document with versions file" in {
    val filePath = "doc/samples/version_document.html"

    val path = Paths.get(filePath)
    Files.deleteIfExists(path)
    val builder = new APIDocumentBuilderMock()
    builder.request(Request.get(URLPath / "test/path"),
                    "",
                    Text("v1"),
                    Version(1, 0, 0, 0))
    builder.request(Request.get(URLPath / "test/path"),
                    "",
                    Text("v2"),
                    Version(2, 0, 0, 0))
    builder.setRootDocumentTitle("version title")
    APIDocumentRenderer.renderTo(builder.getRootAPIDocument, filePath)
    Files.exists(path) should be(true)
  }
  it should "can generate api document file" in {
    val filePath = "doc/samples/empty_document.html"
    val path = Paths.get(filePath)
    Files.deleteIfExists(path)
    APIDocumentRenderer.renderTo(RootAPIDocument("empty doc title", Map()),
                                 filePath)
    Files.exists(path) should be(true)
  }

  it should "can generate only version 0.0.0.0 api document" in {
    val filePath = "doc/samples/version0.0.0.0document.html"

    val path = Paths.get(filePath)
    Files.deleteIfExists(path)
    APIDocumentRenderer.renderTo(getSampleDocument, filePath)
    Files.exists(path) should be(true)

  }

  it should "can generate api document" in {
    val rootAPIDocument = getSampleDocument

    APIDocumentRendererInternal
      .render(rootAPIDocument, APIDocumentRendererContext()) should not be empty

  }

  it should "can output document" in {
    val text = "test out put"
    val dir = Files.createTempDirectory("sample")
    val outputPath = dir + "testFile"
    val path = Paths.get(outputPath)

    Files.deleteIfExists(path)
    APIDocumentRendererInternal.outputDocument(text, outputPath)
    Files.exists(path) should be(true)
    val source = Source.fromFile(outputPath)
    val actualText = new String(source.toArray)
    actualText should be(text)
  }
}
