import com.google.inject.AbstractModule
import java.time.Clock

import net.codingwell.scalaguice.ScalaModule
import repositories.{HospitalPatientRepository, HospitalPatientRepositoryImpl, HospitalRepository, HospitalRepositoryImpl, HospitalReviewRepository, HospitalReviewRepositoryImpl, HospitalStayRepository, HospitalStayRepositoryImpl, HospitalWardRepository, HospitalWardRepositoryImpl, UserRepository, UserRepositoryImpl}
import javax.inject._
import services.{HospitalReviewService, HospitalReviewServiceImpl, HospitalService, HospitalServiceImpl, HospitalStayService, HospitalStayServiceImpl, HospitalWardService, HospitalWardServiceImpl, UserService, UserServiceImpl}

/**
 * This class is a Guice module that tells Guice how to bind several
 * different types. This Guice module is created when the Play
 * application starts.

 * Play will automatically use any class called `Module` that is in
 * the root package. You can create modules in other locations by
 * adding `play.modules.enabled` settings to the `application.conf`
 * configuration file.
 */
class Module extends AbstractModule with ScalaModule {

  override def configure() = {
    // Use the system clock as the default implementation of Clock
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)
    // Ask Guice to create an instance of ApplicationTimer when the

    bind[HospitalRepository].to[HospitalRepositoryImpl].in[Singleton]
    bind[HospitalWardRepository].to[HospitalWardRepositoryImpl].in[Singleton]
    bind[HospitalStayRepository].to[HospitalStayRepositoryImpl].in[Singleton]
    bind[HospitalReviewRepository].to[HospitalReviewRepositoryImpl].in[Singleton]
    bind[HospitalPatientRepository].to[HospitalPatientRepositoryImpl].in[Singleton]
    bind[UserRepository].to[UserRepositoryImpl].in[Singleton]

    bind[HospitalService].to[HospitalServiceImpl].in[Singleton]
    bind[HospitalWardService].to[HospitalWardServiceImpl].in[Singleton]
    bind[HospitalStayService].to[HospitalStayServiceImpl].in[Singleton]
    bind[HospitalReviewService].to[HospitalReviewServiceImpl].in[Singleton]
    bind[UserService].to[UserServiceImpl].in[Singleton]
  }

}
