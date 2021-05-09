package model.dto.hospitalstay

import java.time.LocalDate

import model.dto.hospitalward.HospitalWardDto
import play.api.libs.json.{Format, Json}

case class HospitalStayDto(id: Long,
                           ward: HospitalWardDto,
                           dateFrom: LocalDate,
                           dateTo: LocalDate)

object HospitalStayDto {
  implicit val format: Format[HospitalStayDto] = Json.format
}
