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

sealed trait GitObject[TYPE <: GitObjectType] {
  def typ: TYPE
}

// TODO should be Array[Byte]
class BlobObj(val content: String) extends GitObject[GitObjectType.Blob.type] {
  def typ = GitObjectType.Blob
}

class TreeObj(val content: List[TreeObj.TreeLeaf]) extends GitObject[GitObjectType.Tree.type] {
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

case class CommitObj(
  tree: String,
  parent: List[String],
  author: String,
  committer: String,
  description: String,
) extends GitObject[GitObjectType.Commit.type] {
  def typ = GitObjectType.Commit
}

object CommitObj {
  private val beginPGPSignature: String = "gpgsig -----BEGIN PGP SIGNATURE-----"
  private val endPGPSignature: String = "-----END PGP SIGNATURE-----"

  def apply(rawContent: Array[Byte]): Either[WyagError, CommitObj] = {
    def isKeyValue(s: String): Boolean = s.startsWith("tree") || s.startsWith("parent") || s.startsWith("author") || s.startsWith("committer")
    val contentLines = StringUtils.bytesToString(rawContent).split("\n")

    // key values items
    def loop(contentLines: List[String]): List[(String, String)] =  contentLines match {
      case head :: tail if isKeyValue(head) =>
        head.split(" ", 2).toList match {
          case key :: value :: Nil => List((key, value)) ++ loop(tail)
          case _ => loop(tail)
        }
      case _ => Nil
    }
    val kv: List[(String, String)] = loop(contentLines.toList)

    for {
      tree <- kv.find(_._1 == "tree").map(_._2).toRight(WyagError("No tree header"))
      author <- kv.find(_._1 == "author").map(_._2).toRight(WyagError("No author header"))
      committer <- kv.find(_._1 == "committer").map(_._2).toRight(WyagError("No committer header"))
      description <- Right(
        contentLines.drop(contentLines.indexOf(endPGPSignature) + 1)
          .filterNot(_.trim.isEmpty)
          .lastOption
          .getOrElse("")
      )
    } yield new CommitObj(
      tree = tree,
      parent = kv.filter(_._1 == "parent").map(_._2),
      author = author,
      committer = committer,
      description = description,
    )
  }
}

object GitObject {
  def apply(content: Array[Byte], typ: GitObjectType): Either[WyagError, GitObject[_]] = typ match {
    case GitObjectType.Blob =>
      Right(new BlobObj(StringUtils.bytesToString(content)))
    case GitObjectType.Tree =>
      Right(TreeObj(content))
    case GitObjectType.Tag =>
      println("tag unimplemented")
      ???
    case GitObjectType.Commit =>
      CommitObj(content)
  }
}
