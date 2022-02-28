val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "eff-try-out",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,



    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.5",

    libraryDependencies += "org.atnos" %% "eff" % "5.22.0",
    autoCompilerPlugins := true,
    scalacOptions += "-Ykind-projector:underscores",

    libraryDependencies += "org.atnos" %% "eff-cats-effect" % "5.23.0"
  )
