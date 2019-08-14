import ammonite.ops._

sealed trait GitObjectType {
  def toByte: Array[Byte] = StringUtils.stringToBytes(toString.toLowerCase)
}

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

sealed trait GitObject {
  def typ: GitObjectType
  def sha1: String
  def serialize: Array[Byte]
}

// TODO should be Array[Byte]
case class BlobObj(val content: String, val sha1: String) extends GitObject {
  def typ = GitObjectType.Blob
  def serialize: Array[Byte] = StringUtils.stringToBytes(content)
}

object BlobObj {
  def store(repo: GitRepository, path: Path): Either[WyagError, BlobObj] =
    for {
      content <- PathUtils.readFile(path)
      sha1 <- repo.writeObject(GitObjectType.Blob, StringUtils.stringToBytes(content), false)
    } yield BlobObj(content, sha1)
}

class TreeObj(val content: Seq[TreeObj.TreeLeaf], val sha1: String) extends GitObject {
  def typ = GitObjectType.Tree
  def serialize: Array[Byte] = TreeObj.serialize(content)

  /** Write real value of nodes to a location */
  def writeTo(path: Path, repo: GitRepository): Either[WyagError, Unit] = {
    ListUtils.sequenceE(
      content.map { leaf =>
        for {
          leafObj <- repo.findObject(leaf.sha1.asString)
          _ <- leafObj match {
            case t: TreeObj =>
              val dirPath = path / leaf.fileName
              if (!exists(dirPath)) mkdir(dirPath) else ()
              WyagError.tryCatch(t.writeTo(dirPath, repo), err => s"Cannot write tree: $err")
            case blob: BlobObj =>
              WyagError.tryCatch(write(path / leaf.fileName, blob.content), err => s"Cannot write blob: $err")
            case o => WyagError.l(s"Could not write object of type ${o.typ}")
          }
        } yield ()
      }
    ).map(_ => ())
  }

}

object TreeObj {
  case class TreeLeaf(mode: String, fileName: String, sha1: FullSHA1)

  def serialize(content: Seq[TreeLeaf]): Array[Byte] =
    content.sortWith(_.fileName < _.fileName)
      .map(leaf => StringUtils.stringToBytes(s"${leaf.mode} ${leaf.fileName}\0") ++ leaf.sha1.asBytes)
      .fold(Array[Byte]())(_ ++ _)

  def apply(rawContent: Array[Byte], sha1: String): TreeObj = {
    def loop(array: Array[Byte]): List[Array[Byte]] = {
      val endOfLine = array.indexOf(0) + 21
      if (array.lift(endOfLine).isDefined)
        array.take(endOfLine) :: loop(array.drop(endOfLine))
      else array :: Nil
    }
    val lines = loop(rawContent).map { line =>
      val mode = StringUtils.bytesToString(line.takeWhile(_ != ' '.toByte))
      val fileName = StringUtils.bytesToString(line.drop(mode.length + 1).takeWhile(_ != 0))
      val sha1 = StringUtils.convertBytesToHex(line.takeRight(20))
      TreeLeaf(mode, fileName, FullSHA1.raw(sha1))
    }
    new TreeObj(lines, sha1)
  }

  private val filteredDirectories = List(".git", "out") // TODO remove the filter 'out' hack
  def store(repo: GitRepository, path: Path): Either[WyagError, TreeObj] = {
    for {
      files <- WyagError.tryCatch(ls(path))
      leafs <- ListUtils.sequenceE(
        files.filter(f => !filteredDirectories.contains(f.last)).map { f =>
          val fileStat = stat(f)
          val mode = PathUtils.permissionToOctal(f)
          if (fileStat.isDir) {
            TreeObj.store(repo, f)
              .map(o => TreeLeaf(mode, f.last, FullSHA1(o.sha1).right.get)) // YOLO
          } else if (fileStat.isFile) {
            BlobObj.store(repo, f)
              .map(o => TreeLeaf(mode, f.last, FullSHA1(o.sha1).right.get)) // YOLO
          } else WyagError.l(s"File $f not committed")
        }
      )
      sha1 <- {
        println(path, leafs.mkString("\n"))
        repo.writeObject(GitObjectType.Tree, serialize(leafs), false)
      }
    } yield {
      new TreeObj(leafs, sha1)
    }
  }

}

case class CommitObj(
  tree: String,
  parent: List[String],
  author: String,
  committer: String,
  description: String,
  sha1: String,
) extends GitObject {
  def typ = GitObjectType.Commit
  def serialize: Array[Byte] = CommitObj.serialize(tree, parent, author, committer, description)

  def writeTo(p: Path, repo: GitRepository): Either[WyagError, Unit] = {
    for {
      obj <- repo.findObject(tree)
      tree <- GitObject.asObjectTree(obj)
    } yield ()
  }
}

object CommitObj {
  private val beginPGPSignature: String = "gpgsig -----BEGIN PGP SIGNATURE-----"
  private val endPGPSignature: String = "-----END PGP SIGNATURE-----"

  def serialize(
    tree: String,
    parent: List[String],
    author: String,
    committer: String,
    description: String,
  ): Array[Byte] = StringUtils.stringToBytes(
    (List("tree" -> tree) ++ parent.map("parent" -> _) :+ ("author", author) :+ ("committer", committer))
      .map(t => s"${t._1} ${t._2}")
      .mkString("\n") + s"\n\n$description\n"
  )

  def apply(rawContent: Array[Byte], sha1: String): Either[WyagError, CommitObj] = {
    val contentLines = StringUtils.bytesToString(rawContent).split("\n").toList
    val kv = getKeyValues(contentLines)

    for {
      tree <- kv.find(_._1 == "tree").map(_._2).toRight(WyagError("No tree header"))
      author <- kv.find(_._1 == "author").map(_._2).toRight(WyagError("No author header"))
      committer <- kv.find(_._1 == "committer").map(_._2).toRight(WyagError("No committer header"))
      description <- Right(getDescription(contentLines))
    } yield new CommitObj(
      tree = tree,
      parent = kv.filter(_._1 == "parent").map(_._2),
      author = author,
      committer = committer,
      description = description,
      sha1 = sha1,
    )
  }

  private val keys = List("tree", "parent", "author", "committer", "type", "tag", "tagger", "object")
  def getKeyValues(contentLines: List[String]): List[(String, String)] = {
    def isKeyValue(s: String): Boolean = keys.find(s.startsWith).isDefined
    def loop(contentLines: List[String]): List[(String, String)] =  contentLines match {
      case head :: tail if isKeyValue(head) =>
        head.split(" ", 2).toList match {
          case key :: value :: Nil => List((key, value)) ++ loop(tail)
          case _ => loop(tail)
        }
      case _ => Nil
    }
    loop(contentLines.toList)
  }

  def getDescription(contentLines: List[String]): String =
    contentLines.drop(contentLines.indexOf(endPGPSignature) + 1)
      .filterNot(_.trim.isEmpty)
      .lastOption
      .getOrElse("")

  private val author = "Guillaume Bersac <bersac_1@hotmail.fr> 1565295746 +0200"
  def store(repo: GitRepository, description: String): Either[WyagError, CommitObj] = for {
    tree <- TreeObj.store(repo, repo.worktree)
    parent <- repo.branchHEAD
    ser = serialize(tree.sha1, List(parent.sha1), author, author, description)
    sha1 <- repo.writeObject(GitObjectType.Commit, ser, false)
  } yield CommitObj(tree.sha1, List(parent.sha1), author, author, description, sha1)

}

case class TagObj(
  `object`: String,
  `type`: GitObjectType,
  tag: Option[String],
  tagger: Option[String],
  description: String,
  sha1: String,
) extends GitObject {
  def typ: GitObjectType = GitObjectType.Tag
  def serialize: Array[Byte] = StringUtils.stringToBytes {
    val s = (List("object" -> `object`) ++ List(("type", `type`.toString.toLowerCase)) ++ tag.map(t => "tag" -> t).toList ++ tagger.map(t => "tagger" -> t).toList)
      .map(t => s"${t._1} ${t._2}")
      .mkString("\n") + s"\n\n$description\n"
    s
  }
}

object TagObj {

  def apply(rawContent: Array[Byte], sha1: String): Either[WyagError, TagObj] = {
    val contentLines = StringUtils.bytesToString(rawContent).split("\n").toList
    val kv = CommitObj.getKeyValues(contentLines)
    for {
      obj <- kv.find(_._1 == "object").map(_._2).toRight(WyagError("No object sha1 in tag"))
      typ <- kv.find(_._1 == "type").map(_._2).toRight(WyagError("No type header")).flatMap(GitObjectType.apply)
      description <- Right(CommitObj.getDescription(contentLines))
    } yield new TagObj(
      `object` = obj,
      `type` = typ,
      tag = kv.find(_._1 == "tag").map(_._2),
      tagger = kv.find(_._1 == "tagger").map(_._2),
      description = description,
      sha1 = sha1,
    )
  }

  def createLightweightTag(repo: GitRepository, name: String, sha1: String): Either[WyagError, GitReference] = {
    val path = repo.gitdir / "refs" / "tags" / name
    for {
      obj <- repo.findObject(sha1)
      _ <- if (exists(path)) WyagError.l(s"Tag $name already exists") else Right(())
      _ <- WyagError.tryCatch(write(path, obj.sha1 + "\n"))
      ref <- GitReference(repo, path)
    } yield ref
  }

  def createTagObject(repo: GitRepository, name: String, sha1: String): Either[WyagError, GitReference] =
    for {
      targetObj <- repo.findObject(sha1)
      tagObj = TagObj(
        `object` = targetObj.sha1,
        `type` = targetObj.typ,
        tag = Some(name),
        tagger = Some("Guillaume Bersac <bersac_1@hotmail.fr> 1563997946 +0200"),
        description = "not implemented yet",
        sha1 = "", // we don't know it yet
      )
      newObjSha1 <- repo.writeObject(tagObj.typ, tagObj.serialize)
      ref <- createLightweightTag(repo, name, newObjSha1)
    } yield ref

}

object GitObject {
  def apply(content: Array[Byte], typ: GitObjectType, sha1: String): Either[WyagError, GitObject] = typ match {
    case GitObjectType.Blob =>
      Right(new BlobObj(StringUtils.bytesToString(content), sha1))
    case GitObjectType.Tree =>
      Right(TreeObj(content, sha1))
    case GitObjectType.Tag =>
      TagObj(content, sha1)
    case GitObjectType.Commit =>
      CommitObj(content, sha1)
  }

  import scala.reflect.runtime.universe._
  def asObjectTree(obj: GitObject): Either[WyagError, TreeObj] = obj match {
    case obj: TreeObj if typeOf[obj.type] =:= typeOf[TreeObj.type] => Right(obj)
    case _ => WyagError.l(s"Object is not of expected type tree")
  }

  def asObjectBlob(obj: GitObject): Either[WyagError, BlobObj] = obj match {
    case obj: BlobObj if typeOf[obj.type] =:= typeOf[BlobObj] => Right(obj)
    case _ => WyagError.l(s"Object is not of expected type blob")
  }

  def asObjectCommit(obj: GitObject): Either[WyagError, CommitObj] = obj match {
    case obj: CommitObj => Right(obj)
    case _ => WyagError.l(s"Object is not of expected type commit")
  }

}
