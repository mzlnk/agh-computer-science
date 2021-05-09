package model.dto.user

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class SignInUserDto(username: String,
                         password: String)

object SignInUserDto {
  implicit val format: Format[SignInUserDto] = Json.format

  implicit val reads: Reads[SignInUserDto] = (
      (JsPath \ "username").read[String] and
        (JsPath \ "password").read[String]
    )(SignInUserDto.apply _)

}
