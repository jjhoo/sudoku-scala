import Dependencies._

organization := "asdf"
name := "Sudoku"
version := "0.0.1"
scalaVersion := "2.11.6"
coverageEnabled := true

libraryDependencies ++= List(
  scalatest,
  junit4,
  specs2,
  junitifc)

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
