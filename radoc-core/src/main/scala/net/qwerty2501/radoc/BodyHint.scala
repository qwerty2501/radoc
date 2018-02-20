package net.qwerty2501.radoc

import io.circe.parser._
import io.circe._
trait BodyHint {

  val typeParameterMap: Map[String, Seq[Parameter]]
  def upgrade(message: Message): BodyHint = {
    ContentType(message.headers) match {
      case ContentType.Json =>
        this match {
          case jsonBodyHint: JsonBodyHint =>
            onJsonBodyHint(message, jsonBodyHint)
          case parameterBodyHint: ParameterBodyHint => parameterBodyHint
          case _                                    => onJsonBodyHint(message, JsonBodyHint())
        }

      case _ => this
    }
  }

  private def onJsonBodyHint(message: Message, jsonBodyHint: JsonBodyHint) =
    new JsonBodyHintMerger().merge(
      parse(message.body.contentText).getOrElse(Json.Null),
      jsonBodyHint
    )
}

private case class ParameterBodyHint(
    typeParameterMap: Map[String, Seq[Parameter]])
    extends BodyHint

object BodyHint {
  val empty: BodyHint = new BodyHint {
    override val typeParameterMap: Map[String, Seq[Parameter]] = Map()
  }
  def apply(): BodyHint = empty

  def onlyTypeParameter(
      typeParameterMap: Map[String, Seq[Parameter]]): BodyHint =
    ParameterBodyHint(typeParameterMap)

  def apply(jsonHint: JsonHint[_]): BodyHint = JsonBodyHint(jsonHint)
}
