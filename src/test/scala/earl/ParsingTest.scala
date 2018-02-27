package earl

import org.scalatest.{FlatSpec, Matchers}

import scalaj.http.BaseHttp

import io.circe.generic.auto._
import io.circe.parser._

class ParsingTest extends FlatSpec with Matchers {
  val datasetsUrl = "http://itmoearl.veeroute.com/datasets"
  val dataset0 = "http://itmoearl.veeroute.com/load_dataset/0"
  val optimize = "http://itmoearl.veeroute.com/optimize"
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

  "The 0-th dataset" should "be loaded and optimized by the second objective" in {
    val http = new BaseHttp(proxyConfig = ProxyDetector.theProxy)
    try {
      val response = http(dataset0).timeout(10000, 3600000).asString
      response.isSuccess shouldBe true
      response.code shouldBe 200
      val parseResult = decode[DatasetReply](response.body)
      parseResult.isRight shouldBe true
      val DatasetReply(functionDescription, firstIndividual) = parseResult.right.get
      assert(functionDescription.size == firstIndividual.fitness.size)
      println(s"First individual: $firstIndividual")

      val query = firstIndividual.constructQuery(Seq(1, 2))
      val queryResponse = http(optimize).header("Content-Type", "application/json").postData(query).timeout(10000, 3600000).asString
      queryResponse.isSuccess shouldBe true
      queryResponse.code shouldBe 200
      val queryParseResult = decode[OptimizeReply](queryResponse.body)
      queryParseResult.isRight shouldBe true
      val queryResult = queryParseResult.right.get.result
      queryResult.fitness.size shouldBe firstIndividual.fitness.size
      println(s"Second individual: $queryResult")
    } finally {
      http(cancel).asString
    }
  }
}
