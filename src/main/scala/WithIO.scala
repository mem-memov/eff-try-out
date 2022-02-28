import cats.effect.{IO, IOApp}
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.atnos.eff.addon.cats.effect._
import org.atnos.eff.syntax.addon.cats.effect._

object WithIO extends IOApp.Simple {
  type AppStack = Fx.fx1[IO[_]]
  import cats.effect.unsafe.implicits.global

  override def run: IO[Unit] = {
    val program: Eff[AppStack, Unit] =
      for
        r <- printLn[AppStack]("Hello Eff!")
      yield r

    program
      .unsafeRunSync(global)

    IO.unit
  }

  type _io[R] = MemberIn[IO[_], R]

  def printLn[R: _io](text: String): Eff[R, Unit] = {
    val action: IO[Unit] = IO.delay(println("1111"))
    IOEffect.fromIO[R, Unit](action)
  }
}
