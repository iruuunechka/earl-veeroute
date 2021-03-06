package earl.util

import java.net.Proxy.Type
import java.net.{InetSocketAddress, Proxy}

import scala.util.{Failure, Success, Try}
import scalaj.http.BaseHttp

object ProxyDetector {
  private def checkWhetherProxyWorks(proxy: Option[Proxy]): Boolean = {
    val http = new BaseHttp(proxyConfig = proxy)
    Try {
      http("http://example.org/").timeout(5000, 5000).asString
    } match {
      case Success(response) => response.isSuccess
      case Failure(_) => false
    }
  }

  private val proxyList = Seq(
    None,
    Some(new Proxy(Type.HTTP, new InetSocketAddress("rain.ifmo.ru", 3128))),
  )

  lazy val theProxy: Option[Proxy] = proxyList
    .find(checkWhetherProxyWorks)
    .getOrElse(throw new IllegalStateException("No proxy works"))
}
