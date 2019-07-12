import ammonite.ops._

object Executor {
  def execute(command: CLICommand): Either[WyagError, String] = command match {
    case CLICommand.Init(p) =>
      val path = p.getOrElse(pwd)
      GitRepository.createRepo(path).map(_ => "Successfully initiated repo")
    case CLICommand.CatFile(typ: GitObjectType, name: String) =>
      for {
        repo <- GitRepository.findRepo()

        content <- {
          val obj = repo.findObject(name)
          obj.map(o => o.content)
        }

      } yield content.toString

    case CLICommand.HashObject(file: Path, typ: Option[GitObjectType], write: Boolean) =>
      ???

    case CLICommand.LsTree(treeId: String) =>
      for {
        repo <- findRepo
        content <- repo.findObject(treeId)
          .flatMap {
            case tree: TreeObj =>
              Right(tree.content
                .map(line => s"${line.mode} ${line.sha1} ${line.path}")
                .mkString("\n")
              )
            case _ => WyagError.l(s"Object $treeId is not a tree")
          }
      } yield content
  }

  private def findRepo: Either[WyagError, GitRepository] = GitRepository.findRepo()
}

object Main {
  def main(args: Array[String]): Unit = {
    CLICommand.argsParse(args).flatMap(Executor.execute) match {
      case Left(err) => println("Error:", err.msg)
      case Right(v) => println(v)
    }
  }
}
