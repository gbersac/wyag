import mill._, scalalib._

object main extends ScalaModule {
  def scalaVersion = "2.12.4"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::ammonite-ops:1.6.9",
    ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  )
}
