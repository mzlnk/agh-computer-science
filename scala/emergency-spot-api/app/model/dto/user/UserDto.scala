package model.dto.user

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class UserDto(id: Long,
                   firstName: String,
                   lastName: String,
                   pesel: String,
                   username: String,
                   patientId: Long)

object UserDto {
  implicit val format: Format[UserDto] = Json.format

  implicit val reads: Reads[UserDto] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "firstName").read[String] and
      (JsPath \ "lastName").read[String] and
      (JsPath \ "pesel").read[String] and
      (JsPath \ "username").read[String] and
      (JsPath \ "patientId").read[Long]
    )(UserDto.apply _)

}
