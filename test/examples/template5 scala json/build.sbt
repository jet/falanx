name := "sample7 scala"

version := "0.1"

scalaVersion := "2.12.7"

PB.targets in Compile := Seq(
  scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
)

libraryDependencies += "com.thesamet.scalapb" %% "scalapb-json4s" % "0.7.1"
