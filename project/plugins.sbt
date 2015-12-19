DefaultOptions.addPluginResolvers

resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.3")

//addSbtPlugin("com.typesafe.sbt" % "sbt-mocha" % "1.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-uglify" % "1.0.3")

addSbtPlugin("com.joescii" % "sbt-jasmine-plugin" % "1.4.1")
