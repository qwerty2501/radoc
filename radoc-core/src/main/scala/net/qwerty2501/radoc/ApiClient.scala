package net.qwerty2501.radoc

trait ApiClient {
  def request(request: Request): Response
}
