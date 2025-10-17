inThisBuild(
  Seq(
    organization := "io.github.martinhh",
    homepage := Some(url("https://github.com/martinhh/scalacheck-derived")),
    licenses := Seq(License.Apache2),
    developers := List(
      Developer(
        "martinhh",
        "Martin Hansen",
        "code@martin-hansen.de",
        url("https://martinhh.github.io/")
      )
    ),
    versionScheme := Some("semver-spec")
  )
)

val productionOnlyOptions = Set("-Wunused:all")

def sharedSettings(scalaV: String = "3.3.7") = Seq(
  scalaVersion := scalaV,
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= productionOnlyOptions.toSeq :+ "-Xfatal-warnings",
  Test / scalacOptions ~= { options =>
    options.filterNot(productionOnlyOptions) :+ "-Wunused:imports"
  },
  libraryDependencies ++= Seq(
    "org.scalacheck" %%% "scalacheck" % "1.19.0",
    "org.scalameta" %%% "munit" % "1.2.1" % Test,
    "org.scalameta" %%% "munit-scalacheck" % "1.2.0" % Test
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
      publish / skip := true
    )

// do not publish root project
publish / skip := true
