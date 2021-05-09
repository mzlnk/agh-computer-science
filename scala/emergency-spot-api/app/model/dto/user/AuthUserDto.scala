package model.dto.user

import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class AuthUserDto(id: Long,
                       username: String)

object AuthUserDto {
  implicit val format: Format[AuthUserDto] = Json.format

  implicit val authUserDtoReads: Reads[AuthUserDto] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "username").read[String]
    )(AuthUserDto.apply _)
}
