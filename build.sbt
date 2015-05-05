name := "space"

organization := "bbdl"

version := "0.1.0"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc()
)

initialCommands := "import bbdl.space._"

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalanlp" %% "breeze" % "0.10",
            "com.typesafe.slick" %% "slick" % "3.0.0",
            // native libraries are not included by default. add this if you want them (as of 0.7)
            // native libraries greatly improve performance, but increase jar sizes.
            "org.scalanlp" %% "breeze-natives" % "0.10"
)

resolvers ++= Seq(
            // other resolvers here
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.2.0"

