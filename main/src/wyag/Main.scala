import scala.util.Try

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
      } yield StringUtils.bytesToString(obj.serialize)

    case CLICommand.LsTree(treeId: String) =>
      for {
        repo <- findRepo
        content <- repo.findObject(treeId)
        tree <- GitObject.asObjectTree(content)
      } yield tree.content.map(line => s"${line.mode} ${line.sha1} ${line.fileName}").mkString("\n")

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

    case CLICommand.ListTags =>
      findRepo.map(repo =>
        Try(ls(repo.gitdir / "refs" / "tags"))
          .getOrElse(List())
          .map(_.last)
          .mkString("\n")
      )

    case CLICommand.WriteTag(tagName, sha1, createTagObject) =>
      for {
        repo <- findRepo
        output <-
          if (createTagObject) TagObj.createTagObject(repo, tagName, sha1)
          else TagObj.createLightweightTag(repo, tagName, sha1)
      } yield s"Created $tagName"

    case CLICommand.RevParse(name) =>
      for {
        repo <- findRepo
        obj <- name.resolveAsObject(repo)
      } yield obj.sha1

    case CLICommand.Commit(description) =>
      for {
        repo <- findRepo
        obj <- CommitObj.store(repo, description)
        _ <- repo.updateHEAD(obj)
      } yield obj.sha1

    case CLICommand.Add(paths) =>
      for {
        repo <- findRepo
        _ <- GitIndex.updateIndex(repo, paths.map(p => repo.worktree / p))
      } yield "Index updated"

  }

  private def findRepo: Either[WyagError, GitRepository] = GitRepository.findRepo()
}

object Main {
  def main(args: Array[String]): Unit = {
    CLICommand.argsParse(args).flatMap(Executor.execute) match {
      case Left(err) => println(s"Error: ${err.msg}")
      case Right(v) => println(v)
    }
  }
}
