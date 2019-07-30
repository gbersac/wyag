case class GitRawName(value: String) {

  def resolveAsObject(repo: GitRepository): Either[WyagError, GitObject] = {
    if (value == "HEAD") repo.HEAD.map(_.resolve)
    else repo.findObject(value)
      .left.flatMap(_ => repo.findReference(value).map(_.resolve))
      .left.flatMap(_ => WyagError.l(s"ambiguous argument '$value': unknown revision or path not in the working tree."))
  }

}

object GitRawName {
  def unapply(s: String): Option[GitRawName] = if (s.nonEmpty) Some(GitRawName(s)) else None
}

case class FullSHA1 private (value: String) extends AnyVal

object FullSHA1 {
  private val sha1Regex = "[0-9a-f]{40}".r
  def apply(s: String): Either[WyagError, FullSHA1] =
    WyagError.cond(sha1Regex.unapplySeq(s).isDefined, new FullSHA1(s), s"$s is not a valid SHA1")
}
