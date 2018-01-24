package net.qwerty2501.radoc

trait APIClient {
  def request(request: Request): Response
}
