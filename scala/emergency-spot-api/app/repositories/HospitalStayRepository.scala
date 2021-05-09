package repositories

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import anorm.SqlParser.get
import anorm._
import javax.inject.{Inject, Singleton}
import model.dto.hospitalstay.NewHospitalStayDto
import model.entity.{HospitalStay, HospitalStayParams}
import play.api.db.DBApi

trait HospitalStayRepository {
  def findAll(params: HospitalStayParams): List[HospitalStay]

  def findById(id: Long): Option[HospitalStay]

  def create(stay: NewHospitalStayDto): HospitalStay
}

@Singleton
class HospitalStayRepositoryImpl @Inject()(dbApi: DBApi)(implicit ec: DatabaseExecutionContext) extends HospitalStayRepository {

  private val db = dbApi.database("default")

  private val hospitalStayParser = {
    get[Long]("hospital_stays.hospital_stay_id") ~
      get[LocalDate]("hospital_stays.date_from") ~
      get[LocalDate]("hospital_stays.date_to") ~
      get[Long]("hospital_stays.hospital_patient_id") ~
      get[Long]("hospital_stays.hospital_ward_id") map {
      case id ~ dateFrom ~ dateTo ~ patientId ~ wardId => HospitalStay(id, dateFrom, dateTo, patientId, wardId)
    }
  }

  override def findAll(params: HospitalStayParams): List[HospitalStay] = {
    db.withConnection { implicit connection =>
      val dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val query: SimpleSql[Row] = SQL(
        """
        select * from hospital_stays
        where
        date_from >= {dateFrom} and
        date_to <= {dateTo} and
        (hospital_patient_id = {hospitalPatientId} or 1={hospitalPatientPresent})
        """)
        .on("dateFrom" -> params.dateFrom.getOrElse(LocalDate.parse("1970-01-01")).format(dtf))
        .on("dateTo" -> params.dateTo.getOrElse(LocalDate.parse("2100-12-31")).format(dtf))
        .on("hospitalPatientId" -> params.patientId)
        .on("hospitalPatientPresent" -> valuePresent(params.patientId))

      query.as(hospitalStayParser.*)
    }
  }

  override def findById(id: Long): Option[HospitalStay] = {
    db.withConnection { implicit connection =>
      val query = SQL("select * from hospital_stays where hospital_stay_id={id}")
        .on("id" -> id)

      query.as(hospitalStayParser.singleOpt)
    }
  }

  override def create(stay: NewHospitalStayDto): HospitalStay = {
    db.withConnection { implicit connection =>
      val query = SQL(
        """
        insert into hospital_stays
        (date_from,date_to,hospital_patient_id,hospital_ward_id)
        values
        ({dateFrom},{dateTo},{patientId},{wardId})
        """)
        .on("dateFrom" -> stay.dateFrom)
        .on("dateTo" -> stay.dateTo)
        .on("patientId" -> stay.patientId)
        .on("wardId" -> stay.hospitalWardId)

      val newId: Option[Long] = query.executeInsert()
      findById(newId.get).orNull
    }
  }

  private def valuePresent[T](value: Option[T]) = {
    println(value)
    value match {
      case None => 1
      case _ => 2
    }
  }

}
