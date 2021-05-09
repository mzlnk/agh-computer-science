package services

import javax.inject.{Inject, Singleton}
import model.dto.user.{NewUserDto, UserDto}
import model.entity.User
import repositories.{HospitalPatientRepository, UserRepository}

trait UserService {
  def findOne(id: Long): Option[UserDto]

  def findByUsername(username: String): Option[UserDto]

  def create(user: NewUserDto): User
}

@Singleton
class UserServiceImpl @Inject()(userRepository: UserRepository,
                                hospitalPatientRepository: HospitalPatientRepository) extends UserService {

  override def findOne(id: Long): Option[UserDto] = {
    userRepository
      .findById(id)
      .map(user => getUserDtoFromUser(user))
  }

  override def findByUsername(username: String): Option[UserDto] = {
    userRepository
      .findByUsername(username)
      .map(user => getUserDtoFromUser(user))
  }

  override def create(user: NewUserDto) = {
    userRepository.create(user)
  }

  private def getUserDtoFromUser(user: User) = {
    val patient = hospitalPatientRepository
      .findById(user.patientId)

    UserDto(
      user.id,
      patient.map(p => p.firstName).orNull,
      patient.map(p => p.lastName).orNull,
      patient.map(p => p.pesel).orNull,
      user.username,
      user.patientId
    )
  }

}
