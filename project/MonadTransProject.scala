import sbt._
import Keys._

object MonadTransProject extends Build
{
  lazy val root = Project("MonadTrans", file(".")) settings(coreSettings : _*)

  lazy val commonSettings: Seq[Setting[_]] = Seq(
    organization := "net.debasishg",
    version := "0.0.1",
    scalaVersion := "2.10.1",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:higherKinds")
  )

  lazy val coreSettings = commonSettings ++ Seq(
    name := "MonadTrans",

    libraryDependencies ++= Seq(
      "junit"              % "junit"            % "4.10"            % "test",
      "org.scalatest"      %% "scalatest"  % "1.9.1"            % "test",
      "org.scalaz"         %% "scalaz-core" % "7.0.0",
      "org.scalaz"         %% "scalaz-effect" % "7.0.0"),


    parallelExecution in Test := false,
    publishTo <<= version { (v: String) => 
      val nexus = "https://oss.sonatype.org/" 
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2") 
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { repo => false },
    pomExtra := (
      <url>https://github.com/debasishg/monadtrans</url>
      <licenses>
        <license>
          <name>Apache 2.0 License</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:debasishg/monadtrans.git</url>
        <connection>scm:git:git@github.com:debasishg/monadtrans.git</connection>
      </scm>
      <developers>
        <developer>
          <id>debasishg</id>
          <name>Debasish Ghosh</name>
          <url>http://debasishg.blogspot.com</url>
        </developer>
      </developers>),
    unmanagedResources in Compile <+= baseDirectory map { _ / "LICENSE" }
  )

}

