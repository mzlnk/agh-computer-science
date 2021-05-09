package model.dto.hospitalreview

import play.api.libs.json.{Format, Json}

case class HospitalReviewDto(id: Long,
                             rating: Double)

object HospitalReviewDto {
  implicit val format: Format[HospitalReviewDto] = Json.format
}
