import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.util.concurrent.TimeUnit

import ammonite.ops._

object GitIndex {
  case class Entry(
    ctime: Long,
    ctime_nsec: Long,
    mtime: Long,
    mtime_nsec: Long,
    dev: Long,
    ino: Long,
    mode: Long,
    uid: Integer,
    gid: Integer,
    size: Long,
    oid: FullSHA1,
    flags: Integer,
    path: RelPath
  ) {
    def serialize: Array[Byte] = {
      val content = Array[Long](
        ctime, ctime_nsec, mtime, mtime_nsec, dev, ino, mode.toLong, uid.toLong, gid.toLong, size
      )
        .map(to32bit)
        .flatten
        .++(oid.asBytes)
        .++(to32bit(flags.toLong).takeRight(2))
        .++(StringUtils.stringToBytes(path.toString))
        .++(Array[Byte](0))
      def loop(c: Array[Byte]): Array[Byte] = if (c.length % 8 == 0) c else loop(c ++ Array[Byte](0))
      loop(content)
    }
  }

  object Entry {
    def apply(repo: GitRepository, path: Path, oid: FullSHA1): Entry = {
      val st = stat.full(path)
      Entry(
        ctime = Files.getAttribute(path.toNIO, "unix:ctime").asInstanceOf[FileTime].to(TimeUnit.SECONDS),
        ctime_nsec = 0L,
        mtime = st.mtime.to(TimeUnit.SECONDS),
        mtime_nsec = 0L,
        dev = Files.getAttribute(path.toNIO, "unix:dev").asInstanceOf[Long],
        ino = Files.getAttribute(path.toNIO, "unix:ino").asInstanceOf[Long],
        mode = st.permissions.value.toLong + 32768L /* octal 100000 in decimal */,
        uid = Files.getAttribute(path.toNIO, "unix:uid").asInstanceOf[Integer],
        gid = Files.getAttribute(path.toNIO, "unix:gid").asInstanceOf[Integer],
        size = st.size,
        oid = oid, // oid of associated object
        flags = List(path.relativeTo(repo.worktree).toString.size.toInt, 0xfff).min,
        path = path.relativeTo(repo.worktree),
      )
    }
  }

  private def to32bit(i: Long): Array[Byte] = Array[Byte](0, 0, 0, 0) ++ BigInt(i).toByteArray takeRight(4)
  // warning does not work if one of the file is a dir
  def updateIndex(repo: GitRepository, toAdd: Seq[Path]): Either[WyagError, Unit] = for {
    objsWithPath <- ListUtils.sequenceE(toAdd.map(path => BlobObj.store(repo, path).map(obj => (obj, path))))
    _ <- {
      val entries: Seq[Entry] = objsWithPath.map { case (obj, path) => Entry(repo, path, FullSHA1.raw(obj.sha1)) }
      val header: Array[Byte] = StringUtils.stringToBytes("DIRC") ++ to32bit(2) ++ to32bit(entries.length)
      val entriesBytes: Array[Byte] = entries
        .sortWith { case (a, b) => a.path.toString < b.path.toString }
        .toArray
        .map(_.serialize)
        .flatten
      val content: Array[Byte] = header ++ entriesBytes
      val sha: Array[Byte] = {
        val md = java.security.MessageDigest.getInstance("SHA-1")
        val str = md.digest(content).map("%02x".format(_)).mkString
        FullSHA1.raw(str).asBytes
      }
      WyagError.tryCatch(write.over(repo.indexFile, content ++ sha), m => s"cannot write index file ${repo.indexFile}")
    }
  } yield ()

}
