import scala.util.Try

case class WyagError(msg: String)

object WyagError {
  def tc[A](f: => A): Either[WyagError, A] = Try(f).toEither.left.map(err => WyagError(err.getMessage))
  def l[A](msg: String): Either[WyagError, A] = Left(WyagError(msg))
}
