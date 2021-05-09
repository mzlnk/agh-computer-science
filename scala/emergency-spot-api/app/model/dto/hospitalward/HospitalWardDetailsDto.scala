package model.dto.hospitalward

import model.dto.hospital.HospitalDto
import play.api.libs.json.{Format, Json}

case class HospitalWardDetailsDto(id: Long,
                                  hospital: HospitalDto,
                                  wardType: String,
                                  capacity: Long,
                                  currentHospitalStays: Long,
                                  averageRating: Double)

object HospitalWardDetailsDto {
  implicit val format: Format[HospitalWardDetailsDto] = Json.format
}
