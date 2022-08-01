import sbt._

ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "fi.koodisuo"

val scalatest = "org.scalatest" %% "scalatest" % "3.2.13" % Test
val scalatestp = "org.scalatestplus" %% "junit-4-13" % "3.2.13.0" % Test
val specs2 = "org.specs2" %% "specs2-junit" % "5.0.7" % Test
val junitifc = "com.github.sbt" % "junit-interface" % "0.13.3" % Test

lazy val sudokuSolver = (project in file("."))
  .settings(
    name := "Sudoku",
    coverageEnabled := true,
    libraryDependencies ++= Seq(scalatest, scalatestp, specs2, junitifc)
  )
