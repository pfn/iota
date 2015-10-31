val platform = "android-21"

lazy val macros = project.settings(androidBuildJar: _*).settings(
  platformTarget := platform,
  crossScalaVersions += "2.11.7",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val root = project.in(file(".")).settings(
  mappings in (Compile,packageBin) <++= mappings in (macros,Compile,packageBin)
).dependsOn(macros % "provided").settings(buildWith(macros):_*)

lazy val explore = project.settings(androidBuildJar: _*).settings(
  platformTarget := platform,
  crossScalaVersions += "2.11.7"
).dependsOn(root).settings(buildWith(root):_*)

androidBuildJar

name := "iota"

organization := "com.hanhuy.android"

version := "0.1-SNAPSHOT"

platformTarget := platform

crossScalaVersions += "2.11.7"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= "-deprecation" ::
//  "-Xprint:typer" ::
//  "-Xprint-types" ::
//  "-uniqid" ::
  Nil

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
