import ammonite.ops._

/**
 * @param worktree root of the repo
 * @param gitdir   root of the .git directory of the repo
 */
class GitRepository(val worktree: Path, val gitdir: Path, config: Path) {

  def findObject(sha: String): Either[WyagError, GitObject[_, _]] =
    for {

      raw <- {
        val path = gitdir / "objects" / sha.substring(0, 2) / sha.substring(2)
        if (exists(path)) {
          val bytes = read.bytes(path)
          Right(ZipUtils.decompress(bytes))
        } else WyagError.l(s"No object which sha is $sha in the repo")
      }

      endOfType = raw.indexOf(' '.toByte)
      typ <- {
        val typeByte = raw.slice(0, endOfType)
        GitObjectType(typeByte)
      }

      val endOfSize = raw.indexOf(0)
      _ <- {
        val size = StringUtils.bytesToString(raw.slice(endOfType + 1, endOfSize)).toInt
        val realSize = raw.length - endOfSize - 1
        if (size == realSize) Right(())
        else WyagError.l(s"Object size is $realSize instead of $size")
      }

    } yield GitObject(raw.drop(endOfSize), typ)

}

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

  def findRepo(path: Path = pwd): Either[WyagError, GitRepository] = {
    val gitdir = path / ".git"
    if (exists(gitdir) && stat(gitdir).isDir)
      GitRepository(path)
    else if (path == root)
      Left(WyagError("No git directory."))
    else
      findRepo(path / up)
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

