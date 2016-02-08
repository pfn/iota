val platform = "android-21"

lazy val macros = project.settings(
  platformTarget := platform,
  crossScalaVersions += "2.11.7",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  exportJars := true,
  scalacOptions in Compile += "-language:experimental.macros"
)

lazy val root = project.in(file(".")).settings(
  mappings in (Compile,packageBin) <++= mappings in (macros,Compile,packageBin),
  scalacOptions in Compile += "-language:experimental.macros"
).dependsOn(macros % "provided").settings(buildWith(macros):_*)

lazy val sample = project.settings(androidBuildJar: _*).settings(
  platformTarget := platform,
  classDirectory in Compile := crossTarget.value / "classes",
  crossScalaVersions += "2.11.7"
).dependsOn(root).settings(buildWith(root):_*)

androidBuildJar

inConfig(Compile)(Defaults.packageTaskSettings(packageSrc, Defaults.packageSrcMappings))

classDirectory in Compile := crossTarget.value / "classes"

name := "iota"

organization := "com.hanhuy.android"

version := "1.0.0"

platformTarget := platform

crossScalaVersions += "2.11.7"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"

scalacOptions ++= "-deprecation" ::
//  "-Xprint:typer" ::
//  "-Xprint-types" ::
//  "-uniqid" ::
  Nil

