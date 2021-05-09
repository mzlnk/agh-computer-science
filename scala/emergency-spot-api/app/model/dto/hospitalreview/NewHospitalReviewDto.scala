package model.dto.hospitalreview

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class NewHospitalReviewDto (hospitalStayId: Long,
                                 rating: Double,
                                 hospitalWardId: Long)

object NewHospitalReviewDto {
  implicit val format: Format[NewHospitalReviewDto] = Json.format

  implicit val reads: Reads[NewHospitalReviewDto] = (
    (JsPath \ "hospitalStayId").read[Long] and
      (JsPath \ "rating").read[Double] and
      (JsPath \ "hospitalWardId").read[Long]
  )(NewHospitalReviewDto.apply _)

}
