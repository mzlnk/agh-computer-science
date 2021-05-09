package model.entity

import anorm.{Macro, ToParameterList}
import play.api.libs.json.{Format, Json}

case class HospitalReview(id: Long,
                          rating: Double,
                          hospitalWardId: Long,
                          hospitalStayId: Long
                         )

object HospitalReview {
  implicit def toParameters: ToParameterList[HospitalReview] = Macro.toParameters[HospitalReview]
  implicit val format : Format[HospitalReview] = Json.format
}

case class HospitalReviewParams(minRating: Option[Double],
                                maxRating: Option[Double],
                                hospitalId: Option[Long])

object HospitalReviewParams
