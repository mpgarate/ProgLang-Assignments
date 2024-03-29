name := "hw3"

version := "1.2"

scalaVersion := "2.11.5"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

parallelExecution in Test := false
