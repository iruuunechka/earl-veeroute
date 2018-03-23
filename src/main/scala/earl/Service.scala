package earl

trait Service {
  trait DatasetReference {
    def number: Int
    def version: Int
    def name: String
    def description: String
  }

  trait OptimizerReference {
    def number: Int
    def name: String
    def description: String
  }

  trait Dataset {
    trait Function {
      def isPublic: Boolean
      def number: Int
      def name: String
      def description: String
    }

    trait Individual {
      def id: String
      def fitness: Map[Function, Double]
      def optimize(optimizer: OptimizerReference, functions: Function*): Individual
    }

    def reference: DatasetReference
    def functions: Seq[Function]
    def individuals: Seq[Individual]
  }

  def optimizers: Seq[OptimizerReference]
  def datasets: Seq[DatasetReference]
  def withDataset[T](dataset: DatasetReference)(function: Dataset => T): T
}
