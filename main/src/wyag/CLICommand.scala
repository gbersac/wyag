import ammonite.ops._
import scala.util.Try

/**
 * CLI for command line interface
 */
sealed trait CLICommand

object CLICommand {

  case class Init(p: Option[Path]) extends CLICommand
  case class CatFile(typ: GitObjectType, name: String) extends CLICommand
  case class HashObject(file: Path, typ: Option[GitObjectType], write: Boolean) extends CLICommand
  case class LsTree(treeId: String) extends CLICommand

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

          case "hash-object" =>
            tail match {
              case PathS(p) :: Nil => Right(HashObject(p, None, false))
              case _ => WyagError.l("")
            }

          case "ls-tree" =>
            tail match {
              case treeId :: Nil => Right(LsTree(treeId))
              case _ => WyagError.l("usage ls-tree treeId")
            }

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
