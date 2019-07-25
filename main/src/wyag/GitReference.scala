import ammonite.ops._

sealed trait GitReference {
  def path: Path
  def repo: GitRepository
  def resolve: GitObject
}

case class GitDirectReference(path: Path, repo: GitRepository, sha1: String) extends GitReference {
  def resolve: GitObject = repo.findObject(sha1)
    .right.getOrElse(throw new Error(s"$sha1 is not a correct file")) // should always be a blob
}

case class GitUndirectReference(path: Path, repo: GitRepository, linkTo: Path) extends GitReference {
  def resolve: GitObject = GitReference(repo, linkTo)
    .right.getOrElse(throw new Error(s"$linkTo is not a reference")) // should always exist
    .resolve
}

object GitReference {

  def apply(repo: GitRepository, path: Path): Either[WyagError, GitReference] = {
    PathUtils.readFile(path)
      .flatMap {content =>
        WyagError.tryCatch(Path(content))
          .fold(
            _ => Right(GitDirectReference(path, repo, content.trim)),
            path => Right(GitUndirectReference(path, repo, path))
          )
      }
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
