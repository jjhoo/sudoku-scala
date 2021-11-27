import sbt._

ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "fi.koodisuo"

val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % Test
val junit4 = "junit" % "junit" % "4.12" % Test
val specs2 = "org.specs2" %% "specs2-junit" % "4.7.0" % Test
val junitifc = "com.novocode" % "junit-interface" % "0.11" % Test

lazy val sudokuSolver = (project in file("."))
  .settings(
    name := "Sudoku",
    coverageEnabled := true,
    libraryDependencies ++= Seq(scalatest, junit4, specs2, junitifc)
  )
