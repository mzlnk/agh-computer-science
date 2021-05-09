package model.entity

import anorm.{Macro, ToParameterList}
import play.api.libs.json.{Format, Json}

case class User(id: Long,
                username: String,
                password: String,
                patientId: Long)

object User {
  implicit def toParameters: ToParameterList[User] = Macro.toParameters[User]
  implicit val format: Format[User] = Json.format
}
