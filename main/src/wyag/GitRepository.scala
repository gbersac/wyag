import ammonite.ops._
import scala.math.BigInt

/**
 * @param worktree root of the repo
 * @param gitdir   root of the .git directory of the repo
 */
class GitRepository(val worktree: Path, val gitdir: Path, config: Path) {

  def findObject(sha: String): Either[WyagError, GitObject[_]] =
    for {

      tuple <- {
        ls(gitdir / "objects" / sha.substring(0, 2))
          .find(_.last.startsWith(sha.substring(2)))
          .map { path =>
            val bytes = read.bytes(path)
            println
            (path.last, ZipUtils.decompress(bytes))
          }
          .toRight(WyagError(s"No object which sha is $sha in the repo"))
      }

      (endLongSha1, raw) = tuple
      longSha1 = sha.substring(0, 2) + endLongSha1
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

      obj <- GitObject(raw.drop(endOfSize + 1), typ, longSha1)

    } yield obj


  /** Return the sha1 of the newly created object (if successful) */
  def writeObject(typ: GitObjectType, content: Array[Byte]): Either[WyagError, String] = {
    val header = typ.toByte ++ StringUtils.stringToBytes(" ") ++
      BigInt(content.length).toByteArray ++ StringUtils.stringToBytes("\0")
    ???
  }

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

