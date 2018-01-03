package net.qwerty2501.radoc
import java.io._

import org.fusesource.scalate._

import scala.util.Try
object APIDocumentRenderer {

  def renderTo(rootAPIDocument: RootAPIDocument,
               outputPath: String): Unit =
    renderTo(rootAPIDocument, outputPath, APIDocumentRendererContext())
  def renderTo(rootAPIDocument: RootAPIDocument,
               outputPath: String,
               context: APIDocumentRendererContext): Unit = {
    APIDocumentRendererInternal.outputDocument(
      APIDocumentRendererInternal.render(rootAPIDocument, context),
      outputPath)
  }

}

private object APIDocumentRendererInternal {
  def outputDocument(document: String, outputPath: String): Unit = {
    val printWriter = new PrintWriter(new File(outputPath))
    printWriter.write(document)
    printWriter.close()

  }

  def render(rootAPIDocument: RootAPIDocument,
             context: APIDocumentRendererContext): String = {

    val engine = new TemplateEngine
    engine.layout(
      context.rootAPIDocumentTemplatePath,
      Map("rootAPIDocument" -> rootAPIDocument, "generateContext" -> context))

  }
}
