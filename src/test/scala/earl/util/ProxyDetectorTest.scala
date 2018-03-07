package earl.util

import org.scalatest.{FlatSpec, Matchers}

import scalaj.http.BaseHttp

class ProxyDetectorTest extends FlatSpec with Matchers {
  "ProxyDetector" should "detect the functioning proxy" in {
    val proxy = ProxyDetector.theProxy
    val http = new BaseHttp(proxyConfig = proxy)
    http("https://gentoo.org/").asString.isSuccess shouldBe true
  }
}
