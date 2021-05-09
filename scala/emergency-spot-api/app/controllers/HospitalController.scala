package controllers

import javax.inject.Inject
import model.dto.user.AuthUserDto
import model.entity.{HospitalParams, HospitalWardParams}

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, JsValue, Json, Reads}
import play.api.mvc.{AbstractController, ControllerComponents}
import services.{HospitalService, HospitalWardService}

import scala.concurrent.ExecutionContext

class HospitalController @Inject()(cc: ControllerComponents,
                                   hospitalService: HospitalService,
                                   hospitalWardService: HospitalWardService)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def findAll(name: Option[String],
              longitude: Option[Double],
              latitude: Option[Double],
              country: Option[String],
              city: Option[String],
              wards: Option[List[String]]) = Action {

    val params = HospitalParams(name, longitude, latitude, country, city, wards)
    Ok(Json.toJson(hospitalService.findAll(params)))
  }

  def findById(id: Long) = Action{
    Ok(Json.toJson(hospitalService.findOne(id)))
  }

  def findHospitalWards(id: Long) = Action {
    val params = HospitalWardParams(Option(List.empty[String]), None, None, Option(id))
    Ok(Json.toJson(hospitalWardService.findAll(params)))
  }

  def postTest() = Action { request =>
    val msg: AuthUserDto = request.body.asJson
        .map(json => json.validate[AuthUserDto])
        .map(result => result.get)
        .get

    Ok("Request: " + msg.username)
  }

}
