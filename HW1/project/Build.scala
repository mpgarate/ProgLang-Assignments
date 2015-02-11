import sbt._
import Keys._

object HW1Build extends Build {
  lazy val root = Project(id = "hw1",
                          base = file("."))

  //lazy val grader = Project(id = "hw1-grader",
  //                          base = file("grader")) dependsOn(root)
}
