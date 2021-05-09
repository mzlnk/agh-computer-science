package repositories

import anorm.SqlParser.get
import anorm._
import javax.inject.{Inject, Singleton}
import model.entity.{HospitalWard, HospitalWardParams}
import play.api.db.DBApi

trait HospitalWardRepository {
  def findAll(params: HospitalWardParams): List[HospitalWard]

  def findById(id: Long): Option[HospitalWard]

  def getCurrentHospitalStays(id: Long): Long

  def getAverageRating(id: Long): Double
}

@Singleton
class HospitalWardRepositoryImpl @Inject()(dbApi: DBApi)(implicit ec: DatabaseExecutionContext) extends HospitalWardRepository {

  private val db = dbApi.database("default")

  private val hospitalWardParser = {
    get[Long]("hospital_wards.hospital_ward_id") ~
      get[Long]("hospital_wards.capacity") ~
      get[String]("hospital_wards.ward_type") ~
      get[Long]("hospital_wards.hospital_hospital_id") map {
      case id ~ capacity ~ wardType ~ hospitalId => HospitalWard(id, capacity, wardType, hospitalId)
    }
  }

  private val currentHospitalStaysParser = {
    get[Option[Long]]("result") map (result => result)
  }

  private val avgRatingParser = {
    get[Option[Double]]("result") map (result => result)
  }

  override def findAll(params: HospitalWardParams): List[HospitalWard] = {
    db.withConnection { implicit connection =>
      var wardSeq: Seq[String] = params.wards.get.toStream.toSeq
      if (wardSeq.isEmpty) {
        wardSeq = List("")
      }

      val query: SimpleSql[Row] = SQL(
        """
        select * from hospital_wards
        where
        capacity >= {minCapacity} and
        capacity <= {maxCapacity} and
        (hospital_hospital_id={hospitalId} or 1={hospitalPresent}) and
        (ward_type in ({wards}) or 1={wardsPresent})
        """)
        .on("minCapacity" -> params.minCapacity.getOrElse(Long.MinValue))
        .on("maxCapacity" -> params.maxCapacity.getOrElse(Long.MaxValue))
        .on("hospitalId" -> params.hospitalId)
        .on("hospitalPresent" -> valuePresent(params.hospitalId))
        .on("wards" -> wardSeq)
        .on("wardsPresent" -> listPresent(params.wards))

      query.as(hospitalWardParser.*)
    }
  }

  override def findById(id: Long): Option[HospitalWard] = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("select * from hospital_wards where hospital_ward_id={id}")
        .on("id" -> id)

      query.as(hospitalWardParser.singleOpt)
    }
  }

  override def getCurrentHospitalStays(id: Long): Long = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL(
        """
        select count(*) as result
        from hospital_stays hs
        where hs.hospital_ward_id = {id}
          and hs.date_from <= CURRENT_DATE()
          and hs.date_to <= CURRENT_DATE
        """)
        .on("id" -> id)

      query.as(currentHospitalStaysParser.single).getOrElse(0)
    }
  }

  override def getAverageRating(id: Long): Double = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL(
        """
        select avg(hr.rating) as result from hospital_stays hs
        join hospital_reviews hr on hs.hospital_stay_id = hr.hospital_stay_id
        where hs.hospital_ward_id = {id}
        """)
        .on("id" -> id)

      query.as(avgRatingParser.single).getOrElse(0)
    }
  }

  private def valuePresent[T](value: Option[T]) = {
    println(value)
    value match {
      case None => 1
      case _ => 2
    }
  }

  private def listPresent[T](value: Option[List[T]]) = {
    value match {
      case None => 1
      case Some(list) => list.length + 1
      case _ => 2
    }
  }

}
