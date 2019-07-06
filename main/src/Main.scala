import ammonite.ops._
import scala.util.Try

object CLI {
  def argsParse(args: Seq[String]): Either[WyagError, Unit] = {
    args match {
      case subCommand :: tail =>
        subCommand match {
          case "init" =>
            val path: Either[WyagError, Path] = tail.lift(0).map(x => WyagError.tc(Path(x))).getOrElse(Right(pwd))
            path.flatMap(p => GitRepository.createRepo(p)).map(_ => ())
          case _ =>  Left(WyagError(s"Unknown command $subCommand"))
        }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    CLI.argsParse(args)
  }
}
