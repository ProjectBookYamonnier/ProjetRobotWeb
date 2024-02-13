name := "TP345_GEN"

version := "0.1"

scalaVersion := "2.13.9"

//scalacOptions ++= Seq("-deprecation","-feature")

libraryDependencies ++= Seq(
    "junit" % "junit" % "4.12"%  Test,
    "com.novocode" % "junit-interface" % "0.11" % Test exclude("junit", "junit-dep"),
    "org.scala-lang.modules" % "scala-parser-combinators_2.13" % "1.1.2",
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.18"
    )
