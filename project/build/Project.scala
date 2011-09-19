import sbt.{ProjectInfo, DefaultProject}

class Project(info:ProjectInfo) extends DefaultProject(info) {
  val twitterRepo = "Twitter Repository" at "http://maven.twttr.com/"
  val finagleCore = "com.twitter" % "finagle-core" % "1.9.0"
}