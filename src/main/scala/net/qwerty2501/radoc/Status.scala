package net.qwerty2501.radoc

case class Status(code: Int) {

  override def toString = Status.reasons.get(this) match {
    case Some(reason)    => reason
    case _ if code < 100 => "Unknown Status"
    case _ if code < 200 => "Informational"
    case _ if code < 300 => "Successful"
    case _ if code < 400 => "Redirection"
    case _ if code < 500 => "Client Error"
    case _ if code < 600 => "Server Error"
    case _               => "Unknown Status"
  }

}

object Status {
  val Continue = Status(100)
  val SwitchingProtocols = Status(101)
  val Processing = Status(102)
  val Ok = Status(200)
  val Created = Status(201)
  val Accepted = Status(202)
  val NonAuthoritativeInformation = Status(203)
  val NoContent = Status(204)
  val ResetContent = Status(205)
  val PartialContent = Status(206)
  val MultiStatus = Status(207)
  val MultipleChoices = Status(300)
  val MovedPermanently = Status(301)
  val Found = Status(302)
  val SeeOther = Status(303)
  val NotModified = Status(304)
  val UseProxy = Status(305)
  val TemporaryRedirect = Status(307)
  val PermanentRedirect = Status(308)
  val BadRequest = Status(400)
  val Unauthorized = Status(401)
  val PaymentRequired = Status(402)
  val Forbidden = Status(403)
  val NotFound = Status(404)
  val MethodNotAllowed = Status(405)
  val NotAcceptable = Status(406)
  val ProxyAuthenticationRequired = Status(407)
  val RequestTimeout = Status(408)
  val Conflict = Status(409)
  val Gone = Status(410)
  val LengthRequired = Status(411)
  val PreconditionFailed = Status(412)
  val RequestEntityTooLarge = Status(413)
  val RequestURITooLong = Status(414)
  val UnsupportedMediaType = Status(415)
  val RequestedRangeNotSatisfiable = Status(416)
  val ExpectationFailed = Status(417)
  val EnhanceYourCalm = Status(420)
  val UnprocessableEntity = Status(422)
  val Locked = Status(423)
  val FailedDependency = Status(424)
  val UnorderedCollection = Status(425)
  val UpgradeRequired = Status(426)
  val PreconditionRequired = Status(428)
  val TooManyRequests = Status(429)
  val RequestHeaderFieldsTooLarge = Status(431)
  val UnavailableForLegalReasons = Status(451)
  val ClientClosedRequest = Status(499)
  val InternalServerError = Status(500)
  val NotImplemented = Status(501)
  val BadGateway = Status(502)
  val ServiceUnavailable = Status(503)
  val GatewayTimeout = Status(504)
  val HttpVersionNotSupported = Status(505)
  val VariantAlsoNegotiates = Status(506)
  val InsufficientStorage = Status(507)
  val NotExtended = Status(510)
  val NetworkAuthenticationRequired = Status(511)

  private val reasons: Map[Status, String] = Map(
    Continue -> "Continue",
    SwitchingProtocols -> "Switching Protocols",
    Processing -> "Processing",
    Ok -> "OK",
    Created -> "Created",
    Accepted -> "Accepted",
    NonAuthoritativeInformation -> "Non-Authoritative Information",
    NoContent -> "No Content",
    ResetContent -> "Reset Content",
    PartialContent -> "Partial Content",
    MultiStatus -> "Multi-Status",
    MultipleChoices -> "Multiple Choices",
    MovedPermanently -> "Moved Permanently",
    Found -> "Found",
    SeeOther -> "See Other",
    NotModified -> "Not Modified",
    UseProxy -> "Use Proxy",
    TemporaryRedirect -> "Temporary Redirect",
    PermanentRedirect -> "Permanent Redirect",
    BadRequest -> "Bad Request",
    Unauthorized -> "Unauthorized",
    PaymentRequired -> "Payment Required",
    Forbidden -> "Forbidden",
    NotFound -> "Not Found",
    MethodNotAllowed -> "Method Not Allowed",
    NotAcceptable -> "Not Acceptable",
    ProxyAuthenticationRequired -> "Proxy Authentication Required",
    RequestTimeout -> "Request Timeout",
    Conflict -> "Conflict",
    Gone -> "Gone",
    LengthRequired -> "Length Required",
    PreconditionFailed -> "Precondition Failed",
    RequestEntityTooLarge -> "Request Entity Too Large",
    RequestURITooLong -> "Request-URI Too Long",
    UnsupportedMediaType -> "Unsupported Media Type",
    RequestedRangeNotSatisfiable -> "Requested Range Not Satisfiable",
    ExpectationFailed -> "Expectation Failed",
    EnhanceYourCalm -> "Enhance Your Calm",
    UnprocessableEntity -> "Unprocessable Entity",
    Locked -> "Locked",
    FailedDependency -> "Failed Dependency",
    UnorderedCollection -> "Unordered Collection",
    UpgradeRequired -> "Upgrade Required",
    PreconditionRequired -> "Precondition Required",
    TooManyRequests -> "Too Many Requests",
    RequestHeaderFieldsTooLarge -> "Request Header Fields Too Large",
    UnavailableForLegalReasons -> "Unavailable For Legal Reasons",
    ClientClosedRequest -> "Client Closed Request",
    InternalServerError -> "Internal Server Error",
    NotImplemented -> "Not Implemented",
    BadGateway -> "Bad Gateway",
    ServiceUnavailable -> "Service Unavailable",
    GatewayTimeout -> "Gateway Timeout",
    HttpVersionNotSupported -> "HTTP Version Not Supported",
    VariantAlsoNegotiates -> "Variant Also Negotiates",
    InsufficientStorage -> "Insufficient Storage",
    NotExtended -> "Not Extended",
    NetworkAuthenticationRequired -> "Network Authentication Required"
  )
}
