import ammonite.ops._
import scala.util.Try

object CLI {
  def argsParse(args: Seq[String]): Either[WyagError, String] = {
    args.toList match {
      case subCommand :: tail =>
        subCommand match {
          case "init" =>
            val path: Either[WyagError, Path] = tail.lift(0).map(x => WyagError.tc(Path(x))).getOrElse(Right(pwd))
            path.flatMap(p => GitRepository.createRepo(p)).map(_ => "Successfully initiated repo")
          case "cat-file" =>
            for {
              repo <- GitRepository.findRepo()

              content <- {
                tail match {
                  case GitObjectType(typ) :: objName :: Nil =>
                    val obj = repo.findObject(objName)
                    obj.map(o => o.content)
                  case _ =>
                    Left(WyagError(s"Should be wyag / object type / object name"))
                }
              }

            } yield content.toString
          case _ =>  WyagError.l(s"Unknown command $subCommand")
        }
      case _ => WyagError.l("Usage `wyag subCommand`")
    }
  }

  def findRepo: Either[WyagError, GitRepository] = GitRepository.findRepo()
}

object Main {
  def main(args: Array[String]): Unit = {
    CLI.argsParse(args) match {
      case Left(err) => println("Error:", err.msg)
      case Right(v) => println(v)
    }
  }
}
