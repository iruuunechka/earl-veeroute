package earl

import io.circe.generic.auto._
import io.circe.{Decoder, DecodingFailure, HCursor}

case class Individual(id: String, fitness: Seq[Double])

object Individual {
  case class FitnessRecord(value: Double, corresponding_function_id: Int)

  implicit val decoder: Decoder[Individual] = (c: HCursor) => {
    for {
      id <- c.downField("id").as[String]
      targetValueCursor = c.downField("target_values").downArray
      targetValueHead <- targetValueCursor.as[FitnessRecord]
      targetValueTail = targetValueCursor.rights.get.map(_.as[FitnessRecord])
    } yield {
      targetValueTail.find(_.isLeft) match {
        case Some(theFailure) => throw theFailure.left.get
        case None =>
          val targetValues = targetValueHead +: targetValueTail.map(_.right.get)
          val indices = targetValues.map(_.corresponding_function_id).distinct
          if (indices.size == targetValues.size && indices.min >= 0 && indices.max < targetValues.size) {
            val map = targetValues.map(fr => (fr.corresponding_function_id, fr.value)).toMap
            val seq = IndexedSeq.tabulate(targetValues.size)(map)
            Individual(id, seq)
          } else {
            throw DecodingFailure("Function IDs are not consecutive different integers starting from zero",
              targetValueCursor.history)
          }
      }
    }
  }
}
