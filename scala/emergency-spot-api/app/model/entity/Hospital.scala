package model.entity


import anorm.{Macro, ToParameterList}
import play.api.libs.json._

case class Hospital(id: Long,
                    name: String,
                    description: String,
                    longitude: Double,
                    latitude: Double,
                    country: String,
                    city: String,
                    street: String,
                    streetNumber: Long
                   )

object Hospital {
  implicit def toParameters: ToParameterList[Hospital] = Macro.toParameters[Hospital]

  implicit val format : Format[Hospital] = Json.format
}

case class HospitalParams(name: Option[String],
                          longitude: Option[Double],
                          latitude: Option[Double],
                          country: Option[String],
                          city: Option[String],
                          wards: Option[List[String]])

object HospitalParams
