enablePlugins(Antlr4Plugin)

ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "br.ufpe.cin"
ThisBuild / organizationName := "Centro de Informática, UFPE"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "miniLisp",

    Antlr4 / antlr4Version := "4.13.2",
    Antlr4 / antlr4PackageName := Some("br.ufpe.cin.minilisp.parser"),
    Antlr4 / antlr4GenListener := false,
    Antlr4 / antlr4GenVisitor := true,
    Antlr4 / antlr4TreatWarningsAsErrors := true,

    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4-runtime" % "4.13.2",
      "org.scalameta" %% "munit" % "1.0.4" % Test
    ),

    Compile / run / fork := true,
    Compile / run / connectInput := true,

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    )
  )
