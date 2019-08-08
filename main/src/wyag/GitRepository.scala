import scala.util.Try
import scala.util.control.NonFatal

import ammonite.ops._
import scala.math.BigInt

/**
 * @param worktree root of the repo
 * @param gitdir   root of the .git directory of the repo
 */
class GitRepository(val worktree: Path, val gitdir: Path, config: Path) {
  val refsDir: Path = gitdir / "refs"

  def findObject(sha: String): Either[WyagError, GitObject] =
    for {

      tuple <- {
        WyagError.tryCatch(ls(gitdir / "objects" / sha.substring(0, 2)))
          .flatMap(
            _.find(_.last.startsWith(sha.substring(2)))
              .map { path =>
                val bytes = read.bytes(path)
                (path.last, ZipUtils.decompress(bytes))
              }
              .toRight(WyagError.l(""))
          )
          .left.map(_ => WyagError(s"No object which sha is $sha in the repo"))
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
  def writeObject(typ: GitObjectType, rawContent: Array[Byte], blockIfExists: Boolean = true): Either[WyagError, String] = {
    val data: Array[Byte] = typ.toByte ++ StringUtils.stringToBytes(s" ${rawContent.length}\0") ++ rawContent
    val sha = {
      val md = java.security.MessageDigest.getInstance("SHA-1")
      md.digest(data).map("%02x".format(_)).mkString
    }
    val path = gitdir / "objects" / sha.substring(0, 2) / sha.substring(2)
    if (exists(path))
      if (blockIfExists) WyagError.l(s"File ${path} already exists") else Right(sha)
    else {
      if (!exists(path / up))
        mkdir(path / up)
      val compressed = ZipUtils.compress(data)
      WyagError.tryCatch(write(path, compressed))
        .map(_ => sha)
        .left.map(err => WyagError(s"Cannot create object ${sha} (file ${err.msg})"))
    }
  }

  def findReference(name: String): Either[WyagError, GitReference] = {
    val testPath = (p: Path) => if (exists(p)) Some(GitReference(this, p)) else None

    testPath(worktree / RelPath(name))
      .orElse(testPath(refsDir / "heads" / name))
      .orElse(testPath(refsDir / "tags" / name))
      .getOrElse(WyagError.l(s"No reference Named $name"))
  }

  def HEAD: Either[WyagError, GitReference] = GitReference(this, gitdir / "HEAD")

}

object GitRepository {

  def apply(path: Path): Either[WyagError, GitRepository] = {
    val worktree = path
    val gitdir = path / ".git"
    val config = path / ".git" / "config"

    if (!(exists(gitdir)))
      WyagError.l(s"Not a Git repository ${gitdir}")
    else if (!(exists(config)))
      WyagError.l(s"Config file missing ${config}")
    else
      Right(new GitRepository(worktree, gitdir, config))
  }

  def findRepo(path: Path = pwd): Either[WyagError, GitRepository] = {
    val gitdir = path / ".git"
    if (exists(gitdir) && stat(gitdir).isDir)
      GitRepository(path)
    else if (path == root)
      WyagError.l("No git directory.")
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
      WyagError.l(s"${gitdir} already exists")
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
