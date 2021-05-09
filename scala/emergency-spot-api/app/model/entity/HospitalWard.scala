package model.entity

import anorm.{Macro, ToParameterList}
import model.entity.HospitalWardType.HospitalWardType
import play.api.libs.json.{Format, Json}

object HospitalWardType extends Enumeration {

  type HospitalWardType = Value

  val ISOLATION = Value("ISOLATION")
  val PEDIATRY = Value("PEDIATRY")
  val CARDIOLOGY = Value("CARDIOLOGY")
  val SURGERY = Value("SURGERY")
  val ONCOLOGY = Value("ONCOLOGY")
  val OPHTHAMOLOGY = Value("OPHTHAMOLOGY")
  val ICU = Value("ICU")
  val ER = Value("ER")

}

case class HospitalWard(id: Long,
                        capacity: Long,
                        wardType: String,
                        hospitalId: Long)

object HospitalWard {
  implicit def toParameters: ToParameterList[HospitalWard] = Macro.toParameters[HospitalWard]
  implicit val format: Format[HospitalWard] = Json.format
}

case class HospitalWardParams(wards: Option[List[String]],
                              minCapacity: Option[Long],
                              maxCapacity: Option[Long],
                              hospitalId: Option[Long]
                             )

object HospitalWardParams
