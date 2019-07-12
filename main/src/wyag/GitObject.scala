import ammonite.ops._

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

class TreeObj(val content: List[TreeObj.TreeLeaf]) extends GitObject[GitObjectType.Tree.type, List[TreeObj.TreeLeaf]] {
  def typ = GitObjectType.Tree
}

object TreeObj {
  case class TreeLeaf(mode: String, path: String, sha1: String)

  def apply(rawContent: Array[Byte]): TreeObj = {
    def loop(array: Array[Byte]): List[Array[Byte]] = {
      val endOfLine = array.indexOf(0) + 21
      if (array.lift(endOfLine).isDefined)
        array.take(endOfLine) :: loop(array.drop(endOfLine))
      else array :: Nil
    }
    val lines = loop(rawContent).map { line =>
        val mode = StringUtils.bytesToString(line.takeWhile(_ != ' '.toByte))
        val path = StringUtils.bytesToString(line.drop(mode.length + 1).takeWhile(_ != 0))
        val sha1 = StringUtils.convertBytesToHex(line.takeRight(20))
        TreeLeaf(mode, path, sha1)
      }
    new TreeObj(lines)
  }

}

object GitObject {
  def apply(content: Array[Byte], typ: GitObjectType): GitObject[_, _] = typ match {
    case GitObjectType.Blob =>
      new BlobObj(StringUtils.bytesToString(content))
    case GitObjectType.Tree =>
      TreeObj(content)
    case GitObjectType.Tag =>
      println("tag unimplemented")
      ???
    case GitObjectType.Commit =>
      println("commit unimplemented")
      ???
  }
}
