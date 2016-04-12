import sbt._
import Keys._

object Dependencies {
  val scalatest = "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"
  val junit4 = "junit" % "junit" % "4.11"
  val specs2 = "org.specs2" % "specs2-junit_2.11" % "3.7.2"
  // "org.specs2" % "specs2_2.11" % "3.7"
  val junitifc = "com.novocode" % "junit-interface" % "0.11" % "test"
}
