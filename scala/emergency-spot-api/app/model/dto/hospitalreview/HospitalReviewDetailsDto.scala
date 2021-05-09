package model.dto.hospitalreview

import model.dto.hospitalward.HospitalWardDto
import play.api.libs.json.{Format, Json}

case class HospitalReviewDetailsDto (id: Long,
                                     rating: Double,
                                     ward: HospitalWardDto)

object HospitalReviewDetailsDto {
  implicit val format: Format[HospitalReviewDetailsDto] = Json.format
}
