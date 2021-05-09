package repositories

import anorm.SqlParser.get
import anorm._
import javax.inject.{Inject, Singleton}
import model.dto.user.NewUserDto
import model.entity.HospitalPatient
import play.api.db.DBApi

trait HospitalPatientRepository {
  def findAll(): List[HospitalPatient]

  def findById(id: Long): Option[HospitalPatient]
}

@Singleton
class HospitalPatientRepositoryImpl @Inject()(dbApi: DBApi)(implicit ec: DatabaseExecutionContext) extends HospitalPatientRepository {

  private val db = dbApi.database("default")

  private val hospitalPatientParser = {
    get[Long]("hospital_patients.hospital_patient_id") ~
      get[String]("hospital_patients.first_name") ~
      get[String]("hospital_patients.last_name") ~
      get[String]("hospital_patients.pesel") map {
      case id ~ firstName ~ lastName ~ pesel => HospitalPatient(id, firstName, lastName, pesel)
    }
  }

  override def findAll(): List[HospitalPatient] = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("select * from hospital_patients")
      query.as(hospitalPatientParser.*)
    }
  }

  override def findById(id: Long): Option[HospitalPatient] = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("select * from hospital_patients where hospital_patient_id={id}")
        .on("id" -> id)

      query.as(hospitalPatientParser.singleOpt)
    }
  }

}