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
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    versionScheme := Some("semver-spec")
  )
)

val productionOnlyOptions = Set("-Xfatal-warnings", "-Wunused:all")

val sharedSettings = Seq(
  scalaVersion := "3.3.4",
  testFrameworks += new TestFramework("munit.Framework"),
  scalacOptions ++= productionOnlyOptions.toSeq,
  Test / scalacOptions ~= { options =>
    options.filterNot(productionOnlyOptions) :+ "-Wunused:imports"
  },
  libraryDependencies ++= Seq(
    "org.scalacheck" %%% "scalacheck" % "1.18.1",
    "org.scalameta" %%% "munit" % "1.0.4" % Test,
    "org.scalameta" %%% "munit-scalacheck" % "1.0.0" % Test
  ),
  pomPostProcess := PomPostProcessor.removeScopedDependencies(sLog.value)
)

lazy val core =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(sharedSettings)
    .settings(
      name := "scalacheck-derived",
      description := "A library providing automatic derivation of scalacheck Arbitrary instances for Scala 3."
    )

// do not publish root project
publish / skip := true
