name := "four-in-a-row-bot"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc"       % "3.0.2",
  "com.h2database"  %  "h2"                % "1.4.196",
  "ch.qos.logback"  %  "logback-classic"   % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.tpolecat" %% "doobie-core-cats" % "0.4.4",
  "org.tpolecat" %% "doobie-h2-cats" % "0.4.4",
  "org.tpolecat" %% "doobie-scalatest-cats" % "0.4.4",
  "com.zenecture" %% "neuroflow-core" % "0.806",
  "com.zenecture" %% "neuroflow-application" % "0.806",
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  "com.github.fommil.netlib" % "all" % "1.1.2" pomOnly(),
  "com.typesafe.akka" %% "akka-actor" % "2.5.4"
)


