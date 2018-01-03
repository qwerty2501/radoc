package net.qwerty2501.radoc
import java.io._

import org.fusesource.scalate._

import scala.util.Try
object APIDocumentGenerator {

  def generateDocument(rootAPIDocument: RootAPIDocument,
                       outputPath: String): Unit =
    generateDocument(rootAPIDocument, outputPath, APIDocumentGenerateContext())
  def generateDocument(rootAPIDocument: RootAPIDocument,
                       outputPath: String,
                       context: APIDocumentGenerateContext): Unit = {
    APIDocumentGeneratorInternal.outputDocument(
      APIDocumentGeneratorInternal.generate(rootAPIDocument, context),
      outputPath)
  }

}

private object APIDocumentGeneratorInternal {
  def outputDocument(document: String, outputPath: String): Unit = {
    val printWriter = new PrintWriter(new File(outputPath))
    printWriter.write(document)
    printWriter.close()

  }

  def generate(rootAPIDocument: RootAPIDocument,
               context: APIDocumentGenerateContext): String = {

    val engine = new TemplateEngine
    engine.layout(
      context.rootAPIDocumentTemplatePath,
      Map("rootAPIDocument" -> rootAPIDocument, "generateContext" -> context))

  }
}
