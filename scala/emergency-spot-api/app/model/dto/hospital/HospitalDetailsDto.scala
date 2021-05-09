package model.dto.hospital

import model.dto.hospitalward.HospitalWardDto
import play.api.libs.json.{Format, Json}

case class HospitalDetailsDto(id: Long,
                              name: String,
                              description: String,
                              longitude: Double,
                              latitude: Double,
                              address: AddressDto,
                              averageRating: Double,
                              wards: List[HospitalWardDto])

object HospitalDetailsDto {
  implicit val format: Format[HospitalDetailsDto] = Json.format
}
