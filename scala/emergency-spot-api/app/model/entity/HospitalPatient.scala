package model.entity

import akka.shapeless.syntax.HListOps
import anorm.{Macro, ToParameterList}
import play.api.libs.json.{Format, Json}

case class HospitalPatient(id: Long,
                           firstName: String,
                           lastName: String,
                           pesel: String,
                          )

object HospitalPatient {
  implicit def toParameters: ToParameterList[HospitalPatient] = Macro.toParameters[HospitalPatient]
  implicit val format: Format[HospitalPatient] = Json.format
}

