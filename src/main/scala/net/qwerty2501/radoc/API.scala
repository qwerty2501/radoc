package net.qwerty2501.radoc

case class API private (identifier: APIIdentifier,
                        request: Request,
                        response: Response)

object API {
  def apply(request: Request, response: Response): API =
    new API(APIIdentifier(), request, response)
}
