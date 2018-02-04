package net.qwerty2501.radoc

import scala.xml._

trait Text {
  private[radoc] def renderHtml(args: HtmlRenderArguments): Node
}

private case class NodeText(node: Node) extends Text {
  private[radoc] override def renderHtml(args: HtmlRenderArguments): Node =
    node
}

private case class CustomText(handler: (HtmlRenderArguments) => Node)
    extends Text {
  private[radoc] override def renderHtml(args: HtmlRenderArguments): Node =
    handler(args)
}

object Text {
  def apply(): Text = apply("")
  def apply(text: String): Text = NodeText(xml.Text(text))
  def apply(node: Node): Text = NodeText(node)
  def apply(handler: (HtmlRenderArguments) => Node): Text =
    CustomText(handler)
}
