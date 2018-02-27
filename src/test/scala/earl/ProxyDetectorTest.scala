package earl

import org.scalatest.{FlatSpec, Matchers}

import scalaj.http.BaseHttp

class ProxyDetectorTest extends FlatSpec with Matchers {
  "ProxyDetector" should "detect the functioning proxy" in {
    val proxy = ProxyDetector.detect
    val http = new BaseHttp(proxyConfig = proxy)
    assert(http("https://gentoo.org/").asString.isSuccess)
  }
}
