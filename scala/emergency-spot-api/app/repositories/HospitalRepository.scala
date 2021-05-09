package repositories

import anorm.SqlParser.get
import anorm._
import javax.inject.{Inject, Singleton}
import model.entity.{Hospital, HospitalParams}
import play.api.db.DBApi

trait HospitalRepository {
  def findAll(params: HospitalParams): List[Hospital]

  def findById(id: Long): Option[Hospital]

  def getAverageRating(id: Long): Double
}

@Singleton
class HospitalRepositoryImpl @Inject()(dbApi: DBApi)(implicit ec: DatabaseExecutionContext) extends HospitalRepository {

  private val db = dbApi.database("default")

  private val hospitalParser = {
    get[Long]("hospitals.hospital_id") ~
      get[String]("hospitals.name") ~
      get[String]("hospitals.description") ~
      get[Double]("hospitals.longitude") ~
      get[Double]("hospitals.latitude") ~
      get[String]("hospitals.country") ~
      get[String]("hospitals.city") ~
      get[String]("hospitals.street") ~
      get[Long]("hospitals.street_number") map {
      case id ~
        name ~
        description ~
        longitude ~
        latitude ~
        country ~
        city ~
        street ~
        streetNumber =>
        Hospital(
          id,
          name,
          description,
          longitude,
          latitude,
          country,
          city,
          street,
          streetNumber
        )
    }
  }

  private val avgRatingParser = {
    get[Option[Double]]("result") map (result => result)
  }

  override def findAll(params: HospitalParams): List[Hospital] = {
    db.withConnection { implicit connection =>
      var wardSeq: Seq[String] = params.wards.get.toStream.toSeq
      if (wardSeq.isEmpty) {
        wardSeq = List("")
      }

      val query: SimpleSql[Row] = SQL(
        """
        select distinct hospital_id, name, description, longitude, latitude, country, city, street, street_number from hospitals
        join hospital_wards hw on hospital_id = hospital_hospital_id
        where
        (name={name} or 1={namePresent}) and
        (longitude={longitude} or 1={longitudePresent}) and
        (latitude={latitude} or 1={latitudePresent}) and
        (country={country} or 1={countryPresent}) and
        (city={city} or 1={cityPresent}) and
        (ward_type in ({wards}) or 1={wardsPresent})
        """)
        .on('wards -> wardSeq)
        .on("name" -> params.name)
        .on("longitude" -> params.longitude)
        .on("latitude" -> params.latitude)
        .on("country" -> params.country)
        .on("city" -> params.city)
        .on("namePresent" -> valuePresent(params.name))
        .on("longitudePresent" -> valuePresent(params.longitude))
        .on("latitudePresent" -> valuePresent(params.latitude))
        .on("countryPresent" -> valuePresent(params.country))
        .on("cityPresent" -> valuePresent(params.city))
        .on("wardsPresent" -> listPresent(params.wards))

      query.as(hospitalParser.*)
    }
  }

  override def findById(id: Long): Option[Hospital] = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("select * from hospitals where hospital_id={id}")
        .on("id" -> id)

      query.as(hospitalParser.singleOpt)
    }
  }


  override def getAverageRating(id: Long): Double = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL(
        """
        select avg(hr.rating) as result from hospital_reviews hr
        join hospital_stays hs on hr.hospital_stay_id = hs.hospital_stay_id
        join hospital_wards hw on hr.hospital_ward_id = hw.hospital_ward_id
        where hw.hospital_hospital_id={id}
        """)
        .on("id" -> id)

      query.as(avgRatingParser.single).getOrElse(0D)
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
