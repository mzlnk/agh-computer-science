package model.dto.hospitalward

import model.dto.hospital.HospitalDto
import play.api.libs.json.{Format, Json}

case class HospitalWardDto (id: Long,
                       hospital: HospitalDto,
                       wardType: String)

object HospitalWardDto {
  implicit val format: Format[HospitalWardDto] = Json.format
}
