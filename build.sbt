inThisBuild(
  Seq(
    organization := "io.github.martinhh",
    homepage := Some(uri("https://github.com/martinhh/scalacheck-derived")),
    licenses := Seq(License.Apache2),
    developers := List(
      Developer(
        "martinhh",
        "Martin Hansen",
        "code@martin-hansen.de",
        uri("https://martinhh.github.io/")
      )
    ),
    versionScheme := Some("semver-spec")
  )
)

val scalaLts = "3.3.8"
val scalaLatest = "3.10.0"

val productionOnlyOptions = Set("-Wunused:all")

def sharedSettings(scalaV: String = scalaLts) = Seq(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaLts, scalaLatest),
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= productionOnlyOptions.toSeq ++ Seq(
    "-Werror"
  ),
  Test / scalacOptions ~= { options =>
    options.filterNot(productionOnlyOptions) :+ "-Wunused:imports"
  },
  libraryDependencies ++= Seq(
    "org.scalacheck" %% "scalacheck" % "1.20.0",
    "org.scalameta" %% "munit" % "1.3.6" % Test,
    "org.scalameta" %% "munit-scalacheck" % "1.3.1" % Test
  ),
  pomPostProcess := PomPostProcessor.removeTestDependencies(sLog.value)
)

lazy val core =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(sharedSettings())
    .settings(
      name := "scalacheck-derived",
      description := "A library providing automatic derivation of scalacheck Arbitrary instances for Scala 3."
    )

lazy val extras =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("extras"))
    .dependsOn(core % "compile->compile;test->test")
    .settings(sharedSettings())
    .settings(
      name := "scalacheck-derived-extras",
      description := "A library providing scalacheck Arbitrary instances for Scala 3 literal and union types."
    )

lazy val test34 =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("test34"))
    .dependsOn(core % "compile->compile;test->test")
    .settings(sharedSettings("3.4.0"))
    .settings(
      name := "scalacheck-derived-test-3.4.0",
      description := "A module for tests using a different scala version (i.e. 3.4.0)",
      crossScalaVersions := Seq("3.4.0", scalaLatest),
      publish / skip := true
    )

lazy val root = (project in file("."))
  .aggregate(core.jvm, core.js, core.native)
  .aggregate(extras.jvm, extras.js, extras.native)
  .aggregate(test34.jvm, test34.js, test34.native)
  .settings(
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
    publish / skip := true
  )
