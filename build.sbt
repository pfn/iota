val platform = "android-24"
val doGeneration = taskKey[Seq[File]]("android-conversions-generator")

lazy val generator = project.settings(exportJars := true, publish := (), PgpKeys.publishSigned := ())

libraryDependencies in generator ++= "org.ow2.asm" % "asm-all" % "5.0.4" ::
  Nil
def runGenerator(generatorCp: Seq[Attributed[File]])
                (srcManaged: File,
                 classpath: Seq[Attributed[File]],
                 androidJar: File): List[File] = {
  import collection.JavaConverters._
  type Generator = {
    def apply(srcManaged: File,
              classpath: java.util.List[File],
              androidJar: File,
              pkg: String): java.util.List[File]
  }
  val loader = sbt.classpath.ClasspathUtilities.toLoader(generatorCp.map(_.data))
  val gen = loader.loadClass("ConversionsGenerator").newInstance().asInstanceOf[Generator]
  gen.apply(srcManaged, classpath.map(_.data).asJava, androidJar, "iota").asScala.toList
}


lazy val macros = project.settings(
  platformTarget := platform,
  crossScalaVersions += "2.11.7",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  exportJars := true,
  scalacOptions in Compile += "-language:experimental.macros"
)

lazy val root = project.in(file(".")).settings(
  mappings in (Compile,packageBin) <++= mappings in (macros,Compile,packageBin),
  mappings in (Compile,packageSrc) <++= mappings in (macros,Compile,packageSrc),
  scalacOptions in Compile += "-language:experimental.macros"
).dependsOn(macros % "compile-internal", generator % "compile-internal")

doGeneration in root := {
  val bcp = (bootClasspath in root).value
  runGenerator((fullClasspath in (generator,Runtime)).value)(
    (sourceManaged in (root,Compile)).value, bcp, bcp.head.data)
}

sourceGenerators in (root,Compile) <+= doGeneration in root

lazy val sample = project.enablePlugins(AndroidJar).settings(
  platformTarget := platform,
  classDirectory in Compile := crossTarget.value / "classes",
  crossScalaVersions += "2.11.7"
).dependsOn(root)

enablePlugins(AndroidJar)

classDirectory in Compile := crossTarget.value / "classes"

name := "iota"

organization := "com.hanhuy.android"

version := "2.0.0-SNAPSHOT"

platformTarget := platform

crossScalaVersions += "2.11.7"

libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, 10)) => "org.scala-lang" % "scala-reflect" % scalaVersion.value :: Nil
  case _ => Nil
})
libraryDependencies in sample += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "compile-internal"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"

scalacOptions ++= "-deprecation" ::
//  "-Ylog-classpath" ::
//  "-Xprint:typer" ::
//  "-Xprint-types" ::
//  "-uniqid" ::
  Nil

scalacOptions in sample ++=
//  "-Ylog-classpath" ::
//    "-Xprint:typer" ::
//    "-Xprint-types" ::
//    "-uniqid" ::
  Nil

