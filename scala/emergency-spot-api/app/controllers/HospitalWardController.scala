package controllers

import javax.inject.Inject
import model.entity.HospitalWardParams
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import services.HospitalWardService

import scala.concurrent.ExecutionContext

class HospitalWardController @Inject()(cc: ControllerComponents,
                                       hospitalWardService: HospitalWardService)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def findAll(wards: Option[List[String]],
              min_capacity: Option[Long],
              max_capacity: Option[Long],
              hospital: Option[Long]) = Action {

    val params = HospitalWardParams(wards, min_capacity, max_capacity, hospital)
    Ok(Json.toJson(hospitalWardService.findAll(params)))
  }

  def findOne(id: Long) = Action {
    Ok(Json.toJson(hospitalWardService.findOne(id)))
  }

}
