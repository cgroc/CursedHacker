//-----------------------------------
// The essentials.
//-----------------------------------

val scala3Version = "3.1.1"

lazy val pirate =
  (project in file("."))
    .enablePlugins(
      ScalaJSPlugin, // Enable the Scala.js
      SbtIndigo      //  Enable Indigo plugin
    )
    .settings( // Standard SBT settings
      name         := "pirate",
      version      := "0.0.1",
      scalaVersion := scala3Version,
      organization := "pirate",
      libraryDependencies ++= Seq(
        "org.scalameta"  %%% "munit"      % "0.7.29" % Test,
        "org.scalacheck" %%% "scalacheck" % "1.15.4" % "test"
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
    )
    .settings( // Indigo specific settings
      showCursor            := true,
      title                 := "The Cursed Pirate",
      gameAssetsDirectory   := "assets",
      windowStartWidth      := 1280,
      windowStartHeight     := 720,
      disableFrameRateLimit := false,
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "indigo-json-circe" % "0.12.1", // Needed for Aseprite & Tiled support
        "io.indigoengine" %%% "indigo"            % "0.12.1", // Important! :-)
        "io.indigoengine" %%% "indigo-extras"     % "0.12.1"  // Important! :-)
      )
    )

addCommandAlias("buildGame", ";compile;fastOptJS;indigoBuild")
addCommandAlias("runGame", ";compile;fastOptJS;indigoRun")
