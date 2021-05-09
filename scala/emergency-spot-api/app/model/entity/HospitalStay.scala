package model.entity

import java.time.LocalDate
import java.util.Calendar

import anorm.{Macro, ToParameterList}
import play.api.libs.json.{Format, Json}

case class HospitalStay(id: Long,
                        dateFrom: LocalDate,
                        dateTo: LocalDate,
                        hospitalPatientId: Long,
                        hospitalWardId: Long
                       )

object HospitalStay {
  implicit def toParameters: ToParameterList[HospitalStay] = Macro.toParameters[HospitalStay]

  implicit val format: Format[HospitalStay] = Json.format
}

case class HospitalStayParams(dateFrom: Option[LocalDate],
                              dateTo: Option[LocalDate],
                              patientId: Option[Long]
                             )

object HospitalStayParams
