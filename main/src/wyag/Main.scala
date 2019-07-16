import ammonite.ops._

object Executor {
  def execute(command: CLICommand): Either[WyagError, String] = command match {

    case CLICommand.Init(p) =>
      val path = p.getOrElse(pwd)
      GitRepository.createRepo(path).map(_ => "Successfully initiated repo")

    case CLICommand.CatFile(typ: GitObjectType, name: String) =>
      for {
        repo <- GitRepository.findRepo()
        obj <- repo.findObject(name)
        blob <- obj match {
          case b: BlobObj => Right(b)
          case _ => WyagError.l("object should be a blob")
        }
      } yield blob.content

    case CLICommand.LsTree(treeId: String) =>
      for {
        repo <- findRepo
        content <- repo.findObject(treeId)
        tree <- GitObject.asObjectTree(content)
      } yield tree.content.map(line => s"${line.mode} ${line.sha1} ${line.path}").mkString("\n")

    case CLICommand.Checkout(commitHash, outputDirectory) =>
      for {
        repo <- findRepo
        obj <- repo.findObject(commitHash)
        commit <- GitObject.asObjectCommit(obj)
        _ <- if (exists(outputDirectory / up) && (!exists(outputDirectory) || ls(outputDirectory).isEmpty)) {
          if (!exists(outputDirectory)) mkdir(outputDirectory) else ()
          commit.writeTo(outputDirectory, repo)
        } else WyagError.l(s"$outputDirectory cannot be created or is not empty")
      } yield commit.toString

    case CLICommand.ShowRef =>
      for {
        repo <- findRepo
        refs <- GitReference.all(repo)
      } yield refs.map(ref => s"${ref.resolve.sha1} ${ref.path.relativeTo(repo.gitdir)}").mkString("\n")

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
