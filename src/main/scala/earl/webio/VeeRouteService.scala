package earl.webio

import earl.Service
import earl.util.ProxyDetector

import io.circe.generic.auto._
import io.circe.parser._

import scalaj.http.{BaseHttp, HttpRequest}

object VeeRouteService extends Service {
  class VeeRouteException(message: String, cause: Throwable) extends RuntimeException {
    def this(message: String) = this(message, null)
  }

  private implicit class HttpRequestParser(val request: HttpRequest) extends AnyVal {
    def decodeOr[T : io.circe.Decoder](onError: String): T = {
      val response = request.asString
      if (!response.isSuccess || response.code != 200) {
        throw new VeeRouteException(s"Unexpected response from the web server\n$response")
      }
      val parseResult = decode[T](response.body)
      parseResult.getOrElse(throw new VeeRouteException(onError, parseResult.left.get))
    }
  }

  private val http = new BaseHttp(proxyConfig = ProxyDetector.theProxy)
  private val datasetsUrl = "http://itmoearl.veeroute.com/datasets"
  private val datasetUrl  = "http://itmoearl.veeroute.com/load_dataset/"
  private val optimizeUrl = "http://itmoearl.veeroute.com/optimize"
  private val cancelUrl   = "http://itmoearl.veeroute.com/cancel"

  private val connectTimeout = 60000
  private val readTimeout = 3600000

  private case class MyDatasetReference(id: Int, version: Int, name: String, description: String) extends DatasetReference {
    override def number: Int = id
  }

  private class MyDataset(val reference: DatasetReference) extends Dataset {
    case class MyFunction(id: Int, name: String, description: String, public: Boolean) extends Function {
      override def number: Int = id
      override def isPublic: Boolean = public
      override def toString: String = s"$id"
    }

    case class TargetValue(corresponding_function_id: Int, value: Double) {
      def toPair: (Function, Double) = id2function(corresponding_function_id) -> value
    }

    case class MyIndividual(id: String, target_values: Seq[TargetValue]) extends Individual {
      // why lazy: at the moment of creation of the first individual, id2function is not ready.
      override lazy val fitness: Map[Function, Double] = target_values.map(_.toPair).toMap
      override def optimize(functions: Function*): Individual = {
        val query = s"""{"result_id":"$id","target_functions":[${functions.map(_.number).mkString(",")}]}"""

        val rv = http(optimizeUrl)
          .header("Content-Type", "application/json")
          .postData(query)
          .timeout(connectTimeout, readTimeout)
          .decodeOr[OptimizeReply]("Could not parse the JSON with the reply to /optimize")
          .result
        individuals += rv
        rv
      }

      override def toString: String = s"Individual(id = $id, fitness = $fitness)"
    }

    case class DatasetReply(target_functions: Seq[MyFunction], result: MyIndividual)
    case class OptimizeReply(result: MyIndividual)

    private val parseResult = http(datasetUrl + reference.number)
      .timeout(connectTimeout, readTimeout)
      .decodeOr[DatasetReply]("Could not parse the JSON with dataset description")

    override val individuals = new scala.collection.mutable.ArrayBuffer[MyIndividual]
    override val functions: Seq[Function] = parseResult.target_functions.toIndexedSeq
    private val id2function = functions.map(f => (f.number, f)).toMap
    individuals += parseResult.result
  }

  override val datasets: Seq[DatasetReference] = {
    http(datasetsUrl)
      .timeout(connectTimeout, readTimeout)
      .decodeOr[Seq[MyDatasetReference]]("Could not parse the JSON with dataset descriptions")
  }

  override def withDataset[T](dataset: DatasetReference)(function: Dataset => T): T = try {
    function(new MyDataset(dataset))
  } finally {
    http(cancelUrl).asString
  }
}
