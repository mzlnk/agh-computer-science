package controllers

import javax.inject.Inject
import model.dto.hospitalreview.{NewHospitalReviewDto, UpdateHospitalReviewDto}
import model.entity.HospitalReviewParams
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import services.HospitalReviewService

import scala.concurrent.ExecutionContext

class HospitalReviewController @Inject()(cc: ControllerComponents,
                                         hospitalReviewService: HospitalReviewService)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def findAll(minRating: Option[Double],
              maxRating: Option[Double],
              hospitalId: Option[Long]) = Action {

    val params = HospitalReviewParams(minRating, maxRating, hospitalId)
    Ok(Json.toJson(hospitalReviewService.findAll(params)))
  }

  def findOne(id: Long) = Action {
    Ok(Json.toJson(hospitalReviewService.findOne(id)))
  }

  def create() = Action { request =>
    val review = request.body.asJson
        .map(json => json.validate[NewHospitalReviewDto])
        .map(result => result.get)
        .get

    Ok(Json.toJson(hospitalReviewService.create(review)))
  }

  def update() = Action { request =>
    val review = request.body.asJson
      .map(json => json.validate[UpdateHospitalReviewDto])
      .map(result => result.get)
      .get

    Ok(Json.toJson(hospitalReviewService.update(review)))
  }

}
