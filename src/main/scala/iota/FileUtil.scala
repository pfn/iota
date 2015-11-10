package iota

import java.io.File

/**
  * @author pfnguyen
  */
private[iota] object FileUtil {
  def relativize(base: File, file: File): Option[String] = {
    if (!base.isDirectory) None
    else {
      val basePath = base.getCanonicalPath
      val filePath = file.getCanonicalPath
      if (filePath startsWith basePath) {
        Option(filePath.substring(basePath.length).dropWhile(_ == File.separatorChar))
      } else None
    }
  }

  def cwd = new File(".")

  def firstPathComponent(path: String): Option[String] = {
    val idx = path.indexOf(File.separatorChar)
    if (idx == -1) None else {
      Some(path.substring(0, idx))
    }
  }

  def file(base: File, path: String) = new File(base, path)

  def target(f: File) = {
    // TODO handle deeper nesting
    (for {
      rel <- relativize(cwd, f)
      fst <- firstPathComponent(rel)
    } yield fst).fold(cwd) { fst =>
      // TODO handle discovering other names such as "bin", "build", "output", etc.
      if ("src" == fst)
        file(file(cwd, "target"), "iota")
      else
        file(file(file(cwd, fst), "target"), "iota")
    }
  }
}
