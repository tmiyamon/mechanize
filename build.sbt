name := "mechanize"

version := "0.1"

organization := "cn.orz.pascal"

scalaVersion := "2.9.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "net.sourceforge.htmlunit" % "htmlunit" % "2.9"

libraryDependencies += "nu.validator.htmlparser" % "htmlparser" % "1.2.1"

scalacOptions ++= Seq("-deprecation")

