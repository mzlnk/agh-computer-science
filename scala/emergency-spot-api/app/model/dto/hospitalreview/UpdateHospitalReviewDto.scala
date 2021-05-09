package model.dto.hospitalreview

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class UpdateHospitalReviewDto (hospitalReviewId: Long,
                                    newRating: Double)

object UpdateHospitalReviewDto {
  implicit val format: Format[UpdateHospitalReviewDto] = Json.format

  implicit val reads: Reads[UpdateHospitalReviewDto] = (
    (JsPath \ "hospitalReviewId").read[Long] and
      (JsPath \ "newRating").read[Double]
  )(UpdateHospitalReviewDto.apply _)

}