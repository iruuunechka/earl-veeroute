package earl

import org.scalatest.{FlatSpec, Matchers}

import scalaj.http.BaseHttp

import io.circe.generic.auto._
import io.circe.parser._

class DatasetParsingTest extends FlatSpec with Matchers {
  val url = "http://itmoearl.veeroute.com/datasets"

  "The list of datasets" should "be parsed well" in {
    val http = new BaseHttp(proxyConfig = ProxyDetector.theProxy)
    val response = http(url).asString
    response.isSuccess shouldBe true
    response.code shouldBe 200
    val parseResult = decode[Seq[DatasetId]](response.body)
    parseResult.isRight shouldBe true
    val datasets = parseResult.right.get
    datasets.size should be >= 10
  }
}
