name := "space"

organization := "bbdl"

version := "0.0.1"

// scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc()
)

initialCommands := "import bbdl.space._"

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalanlp" %% "breeze" % "0.10",
            // native libraries are not included by default. add this if you want them (as of 0.7)
            // native libraries greatly improve performance, but increase jar sizes.
            "org.scalanlp" %% "breeze-natives" % "0.10"
)

resolvers ++= Seq(
            // other resolvers here
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

// Scala 2.9.2 is still supported for 0.2.1, but is dropped afterwards.
//scalaVersion := "2.11.1" // or 2.10.3 or later
libraryDependencies += "org.apache.spark" %% "spark-core" % "1.2.0"

