/*
 * Copyright 2023 Paulius Dilkas (National University of Singapore)
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.jar.{Attributes, Manifest}
import Path.makeString
import System._
import java.util.Date

val buildName         = "crane"
val buildOrganization = "edu.ucla.cs.starai"
val buildScalaVersion = "2.11.12"
val buildScalaVersionMajor = "2.11"
val jreTargetVersion  = "1.8"
val buildVersion      = "1.0"
val buildMainClass    = "edu.ucla.cs.starai.forclift.cli.CLI"
val buildJarName      = buildName+".jar"
val buildJarNameDebug = buildName+"-debug"+".jar"
val javacFlags = Seq("-source", jreTargetVersion, "-target", jreTargetVersion)
val productionScalacFlags = Seq(
  "-target:jvm-"+jreTargetVersion,
  "-encoding", "UTF8",
  "-optimise",
  "-Xelide-below", "3000",
  "-Xdisable-assertions"
)
val testScalacFlags = Seq(
  "-target:jvm-"+jreTargetVersion,
  "-encoding", "UTF8",
  "-optimise"
)

lazy val appSettings = Seq (
  name         := buildName,
  organization := buildOrganization,
  scalaVersion := buildScalaVersion,
  version      := buildVersion
)
  
lazy val compileSettings = Seq(
  // Compile options
  Compile / mainClass := Some(buildMainClass),
  // disable assertions and optimize in final binaries
  javacOptions ++= javacFlags,
  Compile / scalacOptions ++= productionScalacFlags
)
  
lazy val testSettings = Seq(
  // show durations and full strack traces (scalatest options)
  Test / testOptions += Tests.Argument("-oDF"),
  javacOptions ++= javacFlags,
  // enable assertions and optimize in test binaries
  Test / scalacOptions := testScalacFlags,
  // do not run tests tagged as 'Slow'
  Test / testOptions += Tests.Argument("-l", "org.scalatest.tags.Slow"),
  // disable parallel testing in an atempt to avoid spurious test errors
  Test / parallelExecution  := false
)
    
lazy val createAllHeaders = addCommandAlias("createAllHeaders",       ";compile:createHeaders;test:createHeaders")

val dependencies = Seq (
  "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test",
  "com.novocode" % "junit-interface" % "0.10-M4" % "test",
  "org.clapper" %% "argot" % "1.0.3",
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "ch.qos.logback" % "logback-classic" % "1.2.10",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "com.lihaoyi" %% "upickle" % "2.0.0"
)

lazy val main = (project in file("."))
  .settings(libraryDependencies := dependencies)
  .settings(appSettings:_*)
  .settings(compileSettings:_*)
  .settings(testSettings:_*)
  .settings(createAllHeaders: _*)

trapExit := false
