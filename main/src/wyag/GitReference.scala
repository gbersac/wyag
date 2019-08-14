import ammonite.ops._

sealed trait GitReference {
  def path: Path
  def repo: GitRepository
  def resolve: GitObject
  def serialize: String
}

case class GitDirectReference(path: Path, repo: GitRepository, sha1: FullSHA1) extends GitReference {
  def resolve: GitObject = repo.findObject(sha1.asString)
    .right.getOrElse(throw new Error(s"$sha1 is not a correct file")) // should always be a blob

  def serialize: String = sha1.asString
}

case class GitUndirectReference(path: Path, repo: GitRepository, linkTo: RelPath) extends GitReference {
  def resolve: GitObject = GitReference(repo, fullPath) match {
    case Right(ref) => ref.resolve
    case Left(err) => throw new Error(s"$linkTo is not a reference (error is ${err.msg})") // should always exist
  }

  def fullPath: Path = repo.gitdir / linkTo

  def serialize: String = s"ref: ${linkTo}"
}

object GitReference {

  def apply(repo: GitRepository, path: Path): Either[WyagError, GitReference] = {
    for {
      content <- PathUtils.readFile(path)
      ref <-
        if (content.startsWith("ref: ")) WyagError.tryCatch(repo.gitdir / RelPath(content.trim.substring(5)))
          .map(linkTo => GitUndirectReference(path, repo, linkTo.relativeTo(repo.gitdir)))
        else FullSHA1(content.trim).map(sha1 => GitDirectReference(path, repo, sha1))
    } yield ref
  }

  def all(repo: GitRepository): Either[WyagError, Seq[GitReference]] = {
    def loop(path: Path): Seq[Either[WyagError, GitReference]] =
      ls(path)
        .flatMap {
          case d if stat(d).isDir => loop(d)
          case f if stat(f).isFile => List(GitReference(repo, f))
          case _ => List.empty[Either[WyagError, GitReference]]
        }
    ListUtils.sequenceE(loop(repo.gitdir / "refs"))
  }

}
