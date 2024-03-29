import ammonite.ops._

object StringUtils {
  def bytesToString(bytes: Array[Byte]): String = bytes.map(_.toChar).mkString
  def stringToBytes(s: String): Array[Byte] = s.toCharArray.map(_.toByte)

  def convertBytesToHex(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }
}

object ListUtils {
  def sequenceE[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldRight(Right(Nil): Either[A, List[B]]) {
      (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }
}

object PathUtils {

  def fromString(s: String): Either[WyagError, Path] =
    WyagError.tryCatch(Path(s), _ => s"Malformed path $s")
      .flatMap { p => if (exists(p)) Right(p) else WyagError.l(s"The file $s does not exists") }

  def readFile(path: Path): Either[WyagError, String] =
    if (!exists(path))
      WyagError.l(s"File $path does not exists.")
    else if (!stat(path).isFile)
      WyagError.l(s"File $path is a ${stat(path).fileType.toString.toLowerCase} not file.")
    else
      WyagError.tryCatch(read(path))

    // TODO symlink
  def permissionToOctal(path: Path): String = {
    val details = stat(path)
    if (details.isDir) "040000"
    else s"100${java.lang.Long.toString(details.permissions.value, 8)}"
  }

}
