import Dependencies._

organization := "asdf"
name := "Sudoku"
version := "0.0.1"
scalaVersion := "2.11.8"
coverageEnabled := true

libraryDependencies ++= List(
  scalatest,
  junit4,
  specs2,
  junitifc)
