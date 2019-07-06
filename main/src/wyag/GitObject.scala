sealed trait GitObjectType

object GitObjectType {
  case object Blob extends GitObjectType
  case object Commit extends GitObjectType
  case object Tag extends GitObjectType
  case object Tree extends GitObjectType

  def apply(bytes: Array[Byte]): Either[WyagError, GitObjectType] = apply(bytes.map(_.toChar).mkString)

  def apply(typ: String): Either[WyagError, GitObjectType] =
    if      (typ.toLowerCase == "commit") Right(Commit)
    else if (typ.toLowerCase == "tree") Right(Tree)
    else if (typ.toLowerCase == "tag") Right(Tag)
    else if (typ.toLowerCase == "blob") Right(Blob)
    else    WyagError.l(s"Unknown object type ${typ}")

  def unapply(typ: String): Option[GitObjectType] = apply(typ).toOption
}

sealed trait GitObject[TYPE <: GitObjectType, CONTENT_TYPE] {
  def typ: TYPE
  def content: CONTENT_TYPE
}

// TODO should be Array[Byte]
class BlobObj(val content: String) extends GitObject[GitObjectType.Blob.type, String] {
  def typ = GitObjectType.Blob
}

object GitObject {
  def apply(content: Array[Byte], typ: GitObjectType): GitObject[_, _] = typ match {
    case GitObjectType.Blob =>
      new BlobObj(StringUtils.bytesToString(content))
    case _ =>
      ???
  }
}
