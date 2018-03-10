import sbt.Keys.scalaVersion

name := "four-in-a-row-bot"



libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc"       % "3.0.2",
  "com.h2database"  %  "h2"                % "1.4.196",
  "ch.qos.logback"  %  "logback-classic"   % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.tpolecat" %% "doobie-core-cats" % "0.4.4",
  "org.tpolecat" %% "doobie-h2-cats" % "0.4.4",
  "org.tpolecat" %% "doobie-scalatest-cats" % "0.4.4",
  "com.zenecture" %% "neuroflow-core" % "1.1.3",// classifier "sources",
  "com.zenecture" %% "neuroflow-application" % "1.1.3",// classifier "sources",
  //"org.scalanlp" %% "breeze-natives" % "0.13.2" classifier "sources",
  "com.github.fommil.netlib" % "all" % "1.1.2" pomOnly(),
  "com.typesafe.akka" %% "akka-actor" % "2.5.4"
)

transitiveClassifiers := Seq("sources")

lazy val core = (project in file("core")).settings(commonSettings)
lazy val learning = (project in file("learning"))
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "org.scalikejdbc" %% "scalikejdbc"       % "3.0.2",
      "com.h2database"  %  "h2"                % "1.4.196",
      "ch.qos.logback"  %  "logback-classic"   % "1.2.3",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "org.tpolecat" %% "doobie-core-cats" % "0.4.4",
      "org.tpolecat" %% "doobie-h2-cats" % "0.4.4",
      "org.tpolecat" %% "doobie-scalatest-cats" % "0.4.4",
      "com.zenecture" %% "neuroflow-core" % "1.1.3",// classifier "sources",
      "com.zenecture" %% "neuroflow-application" % "1.1.3",// classifier "sources",
      //"org.scalanlp" %% "breeze-natives" % "0.13.2" classifier "sources",
      "com.github.fommil.netlib" % "all" % "1.1.2" pomOnly(),
      "com.typesafe.akka" %% "akka-actor" % "2.5.4")
  ).dependsOn(core, riddles)
lazy val riddles = (project in file("riddles.io"))
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "com.zenecture" %% "neuroflow-core" % "1.1.3" % "test",
      "com.zenecture" %% "neuroflow-application" % "1.1.3" % "test"
    )
  )
  .dependsOn(core)

lazy val commonSettings = Seq(version := "0.1", scalaVersion := "2.12.3", libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test")
