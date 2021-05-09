package model.dto.hospital

import play.api.libs.json.{Format, Json}

case class AddressDto (country: String,
                       city: String,
                       street: String,
                       streetNumber: Long)

object AddressDto {
  implicit val format: Format[AddressDto] = Json.format
}
