import sbt._

object PluginDef extends Build {
  lazy val root              = Project("plugins", file(".")) dependsOn(buildPlugin)
  lazy val buildPlugin       = uri("git://github.com/lift/sbt-lift-build.git#724fb133beac77bbd06d3fb8ea086a1c88ee2a7d")
  // lazy val yuiCompressorPlugin = uri("git://github.com/indrajitr/sbt-yui-compressor.git#89304ec0c988183d1f1a889e665e0269fe513031")
  // lazy val yuiCompressorPlugin = uri("git://github.com/eltimn/sbt-yui-compressor.git#371d7917f4414505fa47ac98bd5700afc23fdfab")
}
