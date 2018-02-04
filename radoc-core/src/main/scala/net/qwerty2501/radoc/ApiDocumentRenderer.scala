package net.qwerty2501.radoc
import java.io._

import scala.xml._
import scala.io.Source
object ApiDocumentRenderer {

  def renderHtmlTo(rootAPIDocument: RootApiDocument, outputPath: String): Unit =
    renderHtmlTo(rootAPIDocument, outputPath, ApiDocumentHtmlRendererContext())
  def renderHtmlTo(rootAPIDocument: RootApiDocument,
                   outputPath: String,
                   context: ApiDocumentHtmlRendererContext): Unit = {
    outputDocument(ApiDocumentHtmlRenderer.render(rootAPIDocument, context), outputPath)
  }

  private[radoc] def outputDocument(document: String,
                                    outputPath: String): Unit = {
    val printWriter = new PrintWriter(new File(outputPath))
    printWriter.write(document)
    printWriter.close()

  }

}
