package model.dto.hospital

import play.api.libs.json.{Format, Json}

case class HospitalDto(id: Long,
                       longitude: Double,
                       latitude: Double,
                       name: String,
                       description: String,
                       address: AddressDto)

object HospitalDto {
  implicit val format: Format[HospitalDto] = Json.format
}
