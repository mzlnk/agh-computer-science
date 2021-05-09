package controllers

import javax.inject.Inject
import model.dto.user.{NewUserDto, SignInUserDto, UserDto}
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import services.UserService

import scala.concurrent.ExecutionContext

class UserController @Inject()(cc: ControllerComponents,
                               userService: UserService)(implicit ec: ExecutionContext) extends AbstractController(cc) {
  def findByUsername(username: String) = Action {
    Ok(Json.toJson(userService.findByUsername(username)))
  }

  def signUp() = Action { request =>
    val user = request.body.asJson
      .map(json => json.validate[NewUserDto])
      .map(result => result.get)
      .get

    Ok(Json.toJson(userService.create(user)))
  }

  def signIn() = Action { request =>
    val user = request.body.asJson
      .map(json => json.validate[SignInUserDto])
      .map(result => result.get)
      .get

    Ok(Json.toJson(userService.findByUsername(user.username)))
  }

}
