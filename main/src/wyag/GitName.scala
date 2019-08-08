import java.math.BigInteger

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

case class FullSHA1 private (asString: String, asBytes: Array[Byte])

object FullSHA1 {
  private val sha1Regex = "[0-9a-f]{40}".r
  def apply(s: String): Either[WyagError, FullSHA1] = {
    WyagError.cond(sha1Regex.unapplySeq(s).isDefined, raw(s), s"$s is not a valid SHA1")
  }

  /** Warning: unsafe */
  def raw(s: String): FullSHA1 = {
    val bytes = new BigInteger(s, 16).toByteArray
    // TODO why is there an unexpected 0 at the beginning of the array ?
    new FullSHA1(s, bytes.takeRight(20))
  }

}
