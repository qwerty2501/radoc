package net.qwerty2501.radoc

case class MessageDocument(name: String,
                           request: Request,
                           response: Response)

object MessageDocument {
  def apply(request: Request, response: Response) =
    new MessageDocument(response.status.toString, request, response)
}
