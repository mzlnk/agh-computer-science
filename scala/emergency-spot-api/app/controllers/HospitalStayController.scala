package controllers

import java.time.LocalDate

import javax.inject.Inject
import model.dto.hospitalstay.NewHospitalStayDto
import model.entity.HospitalStayParams
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import services.HospitalStayService

import scala.concurrent.ExecutionContext

class HospitalStayController @Inject()(cc: ControllerComponents,
                                       hospitalStayService: HospitalStayService)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def findAll(dateFrom: Option[String],
              dateTo: Option[String],
              hospitalPatientId: Option[Long]) = Action {
    val params = HospitalStayParams(dateFrom.map(d => LocalDate.parse(d)), dateTo.map(d => LocalDate.parse(d)), hospitalPatientId)
    Ok(Json.toJson(hospitalStayService.findAll(params)))
  }

  def findOne(id: Long) = Action {
    Ok(Json.toJson(hospitalStayService.findOne(id)))
  }

  def create() = Action { request =>
    val stay = request.body.asJson
      .map(json => json.validate[NewHospitalStayDto])
      .map(result => result.get)
      .get

    Ok(Json.toJson(hospitalStayService.create(stay)))
  }

}
