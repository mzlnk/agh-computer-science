name := "emergencyspotapi"
 
version := "1.0" 
      
lazy val `emergencyspotapi` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "https://repo.akka.io/snapshots/"
      
scalaVersion := "2.12.2"

libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice , evolutions )

libraryDependencies += "org.playframework.anorm" %% "anorm" % "2.6.2"
libraryDependencies += "net.codingwell" %% "scala-guice" % "4.2.1"
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.12"
libraryDependencies += "com.github.t3hnar" %% "scala-bcrypt" % "4.1"

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )

      