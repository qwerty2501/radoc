package net.qwerty2501.radoc

import io.circe.parser._
import io.circe._
trait BodyHint {

  val typeParameterMap: Map[String, Seq[Parameter]]
  def upgrade(message: Message): BodyHint = {
    ContentType(message.headers) match {
      case ContentType.Json =>
        new JsonBodyHintMerger().merge(
          parse(message.body.contentText).getOrElse(Json.Null),
          this match {
            case jsonBodyHint: JsonBodyHint => jsonBodyHint
            case _                          => JsonBodyHint()
          })

      case _ => this
    }
  }
}

object BodyHint {
  val empty: BodyHint = new BodyHint {
    override val typeParameterMap: Map[String, Seq[Parameter]] = Map()
  }
  def apply(): BodyHint = empty
}
