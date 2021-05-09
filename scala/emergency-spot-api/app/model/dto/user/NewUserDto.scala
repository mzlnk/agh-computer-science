package model.dto.user

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class NewUserDto(username: String,
                      firstName: String,
                      lastName: String,
                      pesel: String,
                      password: String)

object NewUserDto {
  implicit val format: Format[NewUserDto] = Json.format

  implicit val reads: Reads[NewUserDto] = (
    (JsPath \ "username").read[String] and
      (JsPath \ "firstName").read[String] and
      (JsPath \ "lastName").read[String] and
      (JsPath \ "pesel").read[String] and
      (JsPath \ "password").read[String]
  )(NewUserDto.apply _)

}
