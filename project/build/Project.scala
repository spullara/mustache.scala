import sbt.{ProjectInfo, DefaultProject}

class Project(info:ProjectInfo) extends DefaultProject(info) {
  val twitterRepo = "Twitter Repository" at "http://maven.twttr.com/"
  val finagleCore = "com.twitter" % "finagle-core" % "1.9.0"
  val sonatypeRepo = "Sonatype" at "https://oss.sonatype.org/content/repositories/releases/"
  val mustacheBuilder = "com.github.spullara.mustache.java" % "builder" % "0.5"
  val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7" % "test"
}