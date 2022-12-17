lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github",
      scalaVersion := "2.13.6"
    )),
    name := "scalatest-example"
  )

libraryDependencies ++= Seq(
  "com.github.tototoshi" %% "scala-csv" % "1.3.10",
  "com.lihaoyi" %% "sourcecode" % "0.3.0",
  "io.crashbox" %% "argparse" % "0.16.2",
  "org.apache.spark" %% "spark-sql" % "3.3.1",
  "org.scalatest" %% "scalatest" % "3.2.9" % Test
)
