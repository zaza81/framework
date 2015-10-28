DefaultOptions.addPluginResolvers

resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")

addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.2.1")
