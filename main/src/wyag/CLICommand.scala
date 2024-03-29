import ammonite.ops._
import scala.util.Try

/**
 * CLI for command line interface
 */
sealed trait CLICommand

object CLICommand {

  case class  Init(p: Option[Path]) extends CLICommand
  case class  CatFile(typ: GitObjectType, name: String) extends CLICommand
  case class  LsTree(treeId: String) extends CLICommand
  case class  Checkout(commitHash: String, outputDirectory: Path) extends CLICommand
  case object ShowRef extends CLICommand
  case object ListTags extends CLICommand
  case class  WriteTag(tagName: String, sha1: String, createTagObject: Boolean) extends CLICommand
  case class  RevParse(value: GitRawName) extends CLICommand
  case class  Commit(name: String) extends CLICommand
  case class  Add(paths: Seq[RelPath]) extends CLICommand

  // TODO use a real cli args parser
  def argsParse(args: Seq[String]): Either[WyagError, CLICommand] = {
    args.toList match {
      case subCommand :: tail =>
        subCommand match {

          case "init" =>
            tail match {
              case PathS(p) :: Nil => Right(Init(Some(p)))
              case Nil => Right(Init(None))
              case _ => WyagError.l("")
            }

          case "cat-file" =>
            tail match {
              case GitObjectType(typ) :: objName :: Nil => Right(CatFile(typ, objName))
              case _ => WyagError.l("")
            }

          case "ls-tree" =>
            tail match {
              case treeId :: Nil => Right(LsTree(treeId))
              case _ => WyagError.l("usage ls-tree treeId")
            }

          case "checkout" =>
            tail match {
              case commitHash :: PathS(outputDirectory) :: Nil => Right(Checkout(commitHash, outputDirectory))
              case _ => WyagError.l("usage checkout commitHash outputDirectory")
            }

          case "show-ref" =>
            Right(ShowRef)

          case "tag" =>
            tail match {
              case Nil => Right(ListTags)
              case tagName :: sha1 :: Nil => Right(WriteTag(tagName, sha1, createTagObject = false))
              case "-a" :: tagName :: sha1 :: Nil => Right(WriteTag(tagName, sha1, createTagObject = true))
              case _ => WyagError.l("usage tag [-a] tagName sha1")
            }

          case "rev-parse" =>
            tail match {
              case GitRawName(name) :: Nil => Right(RevParse(name))
              case _ => WyagError.l("usage rev-parse tagName")
            }

          case "commit" =>
            tail match {
              case "-m" :: name :: Nil => Right(Commit(name))
              case _ => WyagError.l("usage commit -m name")
            }

          case "add" =>
            ListUtils.sequenceE(tail.map(s => WyagError.tryCatch(RelPath(s))))
              .map(p => Add(p))

          case _ =>  WyagError.l(s"Unknown command $subCommand")
        }

      case _ => WyagError.l("Usage `wyag subCommand`")
    }
  }

  def optionValue(args: Seq[String], value: String): Option[String] =
    if (args.contains(value)) args.lift(args.indexOf(value) + 1)
    else None

  object PathS {
    def unapply(s: String): Option[Path] = Try(Path(s)).toOption
  }

}
