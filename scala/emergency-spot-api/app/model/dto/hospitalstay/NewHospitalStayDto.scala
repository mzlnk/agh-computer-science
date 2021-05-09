package model.dto.hospitalstay

import java.time.LocalDate

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class NewHospitalStayDto(hospitalWardId: Long,
                              patientId: Long,
                              dateFrom: LocalDate,
                              dateTo: LocalDate)

object NewHospitalStayDto {
  implicit val format: Format[NewHospitalStayDto] = Json.format

  implicit val reads: Reads[NewHospitalStayDto] = (
    (JsPath \ "hospitalWardId").read[Long] and
      (JsPath \ "patientId").read[Long] and
      (JsPath \ "dateFrom").read[LocalDate] and
      (JsPath \ "dateTo").read[LocalDate]
  )(NewHospitalStayDto.apply _)

}
