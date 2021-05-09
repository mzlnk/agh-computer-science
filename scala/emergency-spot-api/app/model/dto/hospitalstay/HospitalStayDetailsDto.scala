package model.dto.hospitalstay

import java.time.LocalDate

import model.dto.hospitalward.HospitalWardDto
import play.api.libs.json.{Format, Json}

case class HospitalStayDetailsDto (id: Long,
                                   ward: HospitalWardDto,
                                   dateFrom: LocalDate,
                                   dateTo: LocalDate)

object HospitalStayDetailsDto {
  implicit val format: Format[HospitalStayDetailsDto] = Json.format
}