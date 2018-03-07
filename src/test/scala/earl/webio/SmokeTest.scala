package earl.webio

import org.scalatest.{FlatSpec, Matchers}

class SmokeTest extends FlatSpec with Matchers {
  "The list of datasets" should "be parsed well" in (VeeRouteService.datasets.size should be >= 10)

  "The 0-th dataset" should "be loaded and optimized by the second objective" in {
    VeeRouteService.withDataset(VeeRouteService.datasets.head) { dataset =>
      dataset.individuals.size shouldEqual 1
      dataset.functions.size should be >= 3

      val first = dataset.individuals.head
      first.fitness.size shouldEqual dataset.functions.size
      println(s"First individual: $first")

      val second = first.optimize(dataset.functions(1), dataset.functions(2))
      second.fitness.size shouldEqual dataset.functions.size
      println(s"Second individual: $second")
    }
  }
}
