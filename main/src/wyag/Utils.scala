object StringUtils {
  def bytesToString(bytes: Array[Byte]): String = bytes.map(_.toChar).mkString

  def convertBytesToHex(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }
}
