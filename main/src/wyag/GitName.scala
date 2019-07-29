case class GitRawName(value: String) {

  def resolveAsObject(repo: GitRepository): Either[WyagError, GitObject] = {
    if (value == "HEAD") ???
    else repo.findObject(value)
      .left.flatMap(_ => repo.findReference(value).map(_.resolve))
      .left.flatMap(_ => WyagError.l(s"ambiguous argument '$value': unknown revision or path not in the working tree."))
  }

}

object GitRawName {
  def unapply(s: String): Option[GitRawName] = if (s.nonEmpty) Some(GitRawName(s)) else None
}
