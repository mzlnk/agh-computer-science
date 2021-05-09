package repositories

import anorm.SqlParser.get
import anorm._
import javax.inject.{Inject, Singleton}
import model.dto.user.NewUserDto
import model.entity.User
import play.api.db.DBApi

import com.github.t3hnar.bcrypt._

trait UserRepository {
  def findAll(): List[User]

  def findById(id: Long): Option[User]

  def findByUsername(username: String): Option[User]

  def create(user: NewUserDto): User
}

@Singleton
class UserRepositoryImpl @Inject()(dbApi: DBApi)(implicit ec: DatabaseExecutionContext) extends UserRepository {

  private val db = dbApi.database("default")

  private val userParser = {
    get[Long]("users.user_id") ~
      get[String]("users.password") ~
      get[String]("users.username") ~
      get[Long]("users.hospital_patient_id") map {
      case id ~ password ~ username ~ patientId => User(id, username, password, patientId)
    }
  }

  override def findAll(): List[User] = {
    db.withConnection { implicit connection =>
      var query: SimpleSql[Row] = SQL("select * from users")
      query.as(userParser.*)
    }
  }

  override def findById(id: Long): Option[User] = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("select * from users where user_id={id}")
        .on("id" -> id)

      query.as(userParser.singleOpt)
    }
  }

  override def findByUsername(username: String): Option[User] = {
    db.withConnection { implicit connection =>
      val query: SimpleSql[Row] = SQL("select * from users where username={username}")
        .on("username" -> username)

      query.as(userParser.singleOpt)
    }
  }

  override def create(user: NewUserDto): User = {
    db.withConnection { implicit connection =>
      val patientQuery: SimpleSql[Row] = SQL("insert into hospital_patients (first_name,last_name,pesel) values ({firstName},{lastName},{pesel})")
        .on("firstName" -> user.firstName)
        .on("lastName" -> user.lastName)
        .on("pesel" -> user.pesel)

      val patientId: Option[Long] = patientQuery.executeInsert()

      val userQuery: SimpleSql[Row] = SQL("insert into users (password,username,hospital_patient_id) values ({password},{username},{patientId})")
        .on("password" -> user.password.bcrypt)
        .on("username" -> user.username)
        .on("patientId" -> patientId.get)

      val userId: Option[Long] = userQuery.executeInsert()
      findById(userId.get).orNull
    }
  }
}
