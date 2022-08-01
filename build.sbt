import sbt._

ThisBuild / scalaVersion := "3.1.3"
ThisBuild / organization := "fi.koodisuo"

val scalatest = "org.scalatest" %% "scalatest" % "3.2.12" % Test
val junit4 = "junit" % "junit" % "4.12" % Test
val specs2 = "org.specs2" %% "specs2-junit" % "5.0.7" % Test
val junitifc = "com.novocode" % "junit-interface" % "0.11" % Test

lazy val sudokuSolver = (project in file("."))
  .settings(
    name := "Sudoku",
    coverageEnabled := true,
    libraryDependencies ++= Seq(scalatest, junit4, specs2, junitifc)
  )
