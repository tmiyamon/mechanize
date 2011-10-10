name := "mechanize"

version := "0.1"

organization := "cn.orz.pascal"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.6.1" % "test",
    "net.sourceforge.htmlunit" % "htmlunit" % "2.9",
    "nu.validator.htmlparser" % "htmlparser" % "1.2.1")

scalacOptions ++= Seq("-deprecation")

