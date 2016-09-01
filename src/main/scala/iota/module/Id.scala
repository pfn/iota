package iota.module

import java.io.{FileOutputStream, OutputStreamWriter}

import scala.language.dynamics
import scala.reflect.macros.Context

/**
  * @author pfnguyen
  */
@deprecated("don't use arbitrary IDs", "2.0.0")
object Id extends Dynamic {
  // identifiers must be consistent for use with id() and findView() --
  // convert Id.x into a literal value that does not change between builds
  def selectDynamic(name: String): Int = macro loadLiteralId

  private[this] val lock = new Object
  // application ID start at 0x7f and above, system ids start at 0x01
  // lets arbitrarily take 0x7e
  // TODO this needs to be made configurable/unique by library...
  private[this] val BASE_ID = 0x7f0dffff
  private[this] var ids = Map.empty[String,Int]
  private[this] var mapTime = 0l

  private[this] val CURRENT_ID_FILE  = "iota-ids-ctr.txt"
  private[this] val EXISTING_ID_FILE = "iota-ids-lst.txt"
  def loadLiteralId(c: Context)(name: c.Expr[String]): c.Expr[Int] = lock.synchronized {
    import c.universe._
    import iota.module.macros.FileUtil._

    val Expr(Literal(Constant(s: String))) = name

    val tgt = target(c.enclosingUnit.source.file.file)
    tgt.mkdirs()

    val currentId = file(tgt, CURRENT_ID_FILE)
    val idsfile = file(tgt, EXISTING_ID_FILE)

    val idMap = loadIds(idsfile)
    val id = idMap.getOrElse(s, {
      if (!currentId.isFile)
        writeCurrentId(currentId, BASE_ID)
      val currentSrc = io.Source.fromFile(currentId)
      val newId = currentSrc.getLines.take(1).next.toInt
      currentSrc.close()
      writeCurrentId(currentId, newId - 1)
      writeNewId(idsfile, s, newId)
      newId
    })

    c.literal(id)
  }

  private[this] def loadIds(idsfile: java.io.File): Map[String,Int] = {
    if (idsfile.lastModified < mapTime) {
      ids
    } else {
      ids = if (idsfile.isFile) {
        val src = io.Source.fromFile(idsfile)
        try {
          src.getLines.map { l =>
            val parts = l.split(' ')
            (parts(0), parts(1).toInt)
          }.toMap
        } finally {
          src.close()
        }
      } else {
        Map.empty
      }
      mapTime = idsfile.lastModified
      ids
    }
  }

  private[this] def writeCurrentId(f: java.io.File, id: Int): Unit = {
    val fout = new OutputStreamWriter(new FileOutputStream(f), "utf-8")
    fout.write(id.toString + "\n")
    fout.close()
  }
  private[this] def writeNewId(f: java.io.File, key: String, id: Int): Unit = {
    val fout = new OutputStreamWriter(new FileOutputStream(f, true), "utf-8")
    fout.write(s"$key $id\n")
    fout.close()
  }
}
