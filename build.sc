import mill._, scalalib._

object aoc21 extends ScalaModule {
  override  def scalaVersion = "3.1.0"

  override def ivyDeps = Agg(
    ivy"io.github.bbstilson::aocd:0.1.3".withDottyCompat(scalaVersion())
  )
}
