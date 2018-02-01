package net.qwerty2501.radoc

import java.io.FileNotFoundException

import scala.io._
import scala.util.Try
import scala.xml._

private object ResourceLoader {
  private def basePath = "net/qwerty2501/radoc"

  private def assets = "assets"

  def loadCss(fileName: String): Node =
    loadTextAsNode(assets + "/css/" + fileName)

  def loadJavaScript(fileName: String): Node =
    loadTextAsNode(assets + "/js/" + fileName)

  private def loadTextAsNode(path: String): Node = Unparsed(loadText(path))

  private def loadText(path: String): String = load(path).getLines.mkString

  private def load(path: String): BufferedSource = {

    val targetPath = basePath + "/" + path
    Try(io.Source.fromResource(targetPath, getClass.getClassLoader)).recover {
      case e => throw new FileNotFoundException(targetPath)
    }.get

  }

}
