object StringUtils {
  def bytesToString(bytes: Array[Byte]): String = bytes.map(_.toChar).mkString
}
