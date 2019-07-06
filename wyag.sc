import ammonite.ops._
import scala.util.Try

case class WyagError(msg: String)

object WyagError {
  def tc[A](f: => A): Either[WyagError, A] = Try(f).toEither.left.map(err => WyagError(err.getMessage))
}

class GitRepository(val worktree: Path, val gitdir: Path, config: Path) {}

object GitRepository {

  def apply(path: Path): Either[WyagError, GitRepository] = {
    val worktree = path
    val gitdir = path / ".git"
    val config = path / ".git" / "config"

    if (!(exists(gitdir)))
      Left(WyagError(s"Not a Git repository ${gitdir}"))
    else if (!(exists(config)))
      Left(WyagError(s"Config file missing ${config}"))
    else
      Right(new GitRepository(worktree, gitdir, config))
  }

  def repoFind(path: Path = pwd): Either[WyagError, GitRepository] = {
    val gitdir = path / ".git"
    if (exists(gitdir) && stat(gitdir).isDir)
      GitRepository(path)
    else if (path == root)
      Left(WyagError("No git directory."))
    else
      repoFind(path / up)
  }

  def createRepo(path: Path): Either[WyagError, GitRepository] = {
    val gitdir = pwd / ".git"
    val defaultConfig = Seq(
      "[core]",
      "repositoryformatversion = 0",
      "filemode = false",
      "bare = false"
    ).mkString("\n")

    if (exists(gitdir))
      Left(WyagError(s"${gitdir} already exists"))
    else {
      mkdir(gitdir)
      mkdir(gitdir / "branches")
      mkdir(gitdir / "objects")
      mkdir(gitdir / "refs" / "tags")
      mkdir(gitdir / "refs" / "heads")

      write(gitdir / "description", "Unnamed repository; edit this file 'description' to name the repository.\n")
      write(gitdir / "HEAD", "ref: refs/heads/master\n")
      write(gitdir / "config", defaultConfig)

      GitRepository(path) match {
        case Left(err) =>
          rm(gitdir)
          Left(err)
        case x => x
      }
    }
  }
}

def argsParse(subCommand: String, args: Seq[String]): Either[WyagError, Unit] = {
  subCommand match {
    case "init" =>
      val path: Either[WyagError, Path] = args.lift(0).map(x => WyagError.tc(Path(x))).getOrElse(Right(pwd))
      path.flatMap(p => GitRepository.createRepo(p)).map(_ => ())
    case _ =>  Left(WyagError(s"Unknown command $subCommand"))
  }
}

@main
def main(subCommand: String, args: String*) = {
  // GitRepository(pwd) match {
  //   case Left(err) => s"error: ${err.msg}"
  //   case Right(rep) => s"${rep.worktree} ${rep.gitdir}"
  // }
  argsParse(subCommand, args)
}
