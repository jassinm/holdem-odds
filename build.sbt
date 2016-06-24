name := """holdem-odds"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

mappings in (Compile, packageBin) <+=
  baseDirectory map { dir => ( dir / "flushes.txt") -> "flushes.txt" }

mappings in (Compile, packageBin) <+=
  baseDirectory map { dir => ( dir / "hashvalues.txt") -> "hashvalues.txt" }

mappings in (Compile, packageBin) <+=
  baseDirectory map { dir => ( dir / "unique5.txt") -> "unique5.txt" }
