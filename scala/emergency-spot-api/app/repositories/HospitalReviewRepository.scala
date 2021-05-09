package repositories

import anorm.SqlParser.get
import anorm._
import javax.inject.{Inject, Singleton}
import model.dto.hospitalreview.{NewHospitalReviewDto, UpdateHospitalReviewDto}
import model.entity.{HospitalReview, HospitalReviewParams}
import play.api.db.DBApi

trait HospitalReviewRepository {
  def findAll(params: HospitalReviewParams): List[HospitalReview]

  def findById(id: Long): Option[HospitalReview]

  def create(review: NewHospitalReviewDto): HospitalReview

  def update(review: UpdateHospitalReviewDto): HospitalReview
}

@Singleton
class HospitalReviewRepositoryImpl @Inject()(dbApi: DBApi)(implicit ec: DatabaseExecutionContext) extends HospitalReviewRepository {

  private val db = dbApi.database("default")

  private val hospitalReviewParser = {
    get[Long]("hospital_reviews.hospital_review_id") ~
      get[Double]("hospital_reviews.rating") ~
      get[Long]("hospital_reviews.hospital_stay_id") ~
      get[Long]("hospital_reviews.hospital_ward_id") map {
      case id ~ rating ~ hospitalStayId ~ hospitalWardId => HospitalReview(id, rating, hospitalWardId, hospitalStayId)
    }
  }

  override def findAll(params: HospitalReviewParams): List[HospitalReview] = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL(
        """
        select distinct hr.hospital_review_id, hr.rating, hr.hospital_stay_id, hr.hospital_ward_id from hospital_review hr
        join hospital_stays hs on hs.hospital_stay_id = hr.hospital_stay_id
        join hospital_wards hw on hw.hospital_ward_id = hs.hospital_ward_id
        where
        (hw.hospital_id={hospitalId} or 1={hospitalPresent}) and
        (hr.rating >= {minRating}) and
        (hr.rating <= {maxRating})
        """)
        .on("hospitalId" -> params.hospitalId)
        .on("hospitalPresent" -> valuePresent(params.hospitalId))
        .on("minRating" -> params.minRating.getOrElse(0D))
        .on("maxRating" -> params.maxRating.getOrElse(10D))

      query.as(hospitalReviewParser.*)
    }
  }

  override def findById(id: Long): Option[HospitalReview] = {
    db.withConnection { implicit connection =>
      var query: SimpleSql[Row] = SQL("select * from hospital_reviews where hospital_review_id={id}")
        .on("id" -> id)

      query.as(hospitalReviewParser.singleOpt)
    }
  }


  override def create(review: NewHospitalReviewDto): HospitalReview = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("insert into hospital_reviews (rating,hospital_stay_id,hospital_ward_id) values ({rating},{hospitalStayId},{hospitalWardId})")
        .on("rating" -> review.rating)
        .on("hospitalStayId" -> review.hospitalStayId)
        .on("hospitalWardId" -> review.hospitalWardId)

      val newId: Option[Long] = query.executeInsert()
      println("id: " + newId)
      findById(newId.get).orNull
    }
  }

  override def update(review: UpdateHospitalReviewDto): HospitalReview = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("update hospital_reviews set rating={rating} where hospital_review_id={id}")
        .on("rating" -> review.newRating)
        .on("id" -> review.hospitalReviewId)

      findById(review.hospitalReviewId).orNull
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
