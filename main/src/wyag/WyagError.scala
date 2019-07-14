import scala.util.Try

case class WyagError(msg: String)

object WyagError {
  def tryCatch[A](f: => A, msg: (String) => String = identity): Either[WyagError, A] =
    Try(f).toEither.left.map(err => WyagError(msg(err.getMessage)))

  def l[A](msg: String): Either[WyagError, A] = Left(WyagError(msg))
}
