package earl

import org.scalatest.{FlatSpec, Matchers}

import scalaj.http.BaseHttp

import io.circe.generic.auto._
import io.circe.parser._

class ParsingTest extends FlatSpec with Matchers {
  val datasetsUrl = "http://itmoearl.veeroute.com/datasets"
  val dataset0 = "http://itmoearl.veeroute.com/load_dataset/0"
  val cancel = "http://itmoearl.veeroute.com/cancel"

  "The list of datasets" should "be parsed well" in {
    val http = new BaseHttp(proxyConfig = ProxyDetector.theProxy)
    val response = http(datasetsUrl).asString
    response.isSuccess shouldBe true
    response.code shouldBe 200
    val parseResult = decode[Seq[DatasetId]](response.body)
    parseResult.isRight shouldBe true
    val datasets = parseResult.right.get
    datasets.size should be >= 10
  }

  "The 0-th dataset" should "be loaded correctly" in {
    val http = new BaseHttp(proxyConfig = ProxyDetector.theProxy)
    try {
      val response = http(dataset0).timeout(10000, 3600000).asString
      response.isSuccess shouldBe true
      response.code shouldBe 200
      val parseResult = decode[DatasetReply](response.body)
      parseResult.isRight shouldBe true
      val DatasetReply(functionDescription, firstIndividual) = parseResult.right.get
      assert(functionDescription.size == firstIndividual.fitness.size)
    } finally {
      http(cancel).asString
    }
  }
}
