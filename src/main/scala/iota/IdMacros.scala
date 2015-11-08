package iota

import java.io.{FileOutputStream, OutputStreamWriter}

import android.view.View

import scala.reflect.ClassTag
import scala.reflect.macros.Context

/**
  * @author pfnguyen
  */
private[iota] object IdMacros {

  case class IdType[+A : ClassTag]()

  implicit def materializeIdType: IdMacros.IdType[Any] = macro matIdType
  def matIdType(c: Context): c.Expr[IdMacros.IdType[Any]] = {
    import c.universe._
    println(c.enclosingImplicits)
    val s = rootMirror.staticClass("java.lang.String")
    val tpe = s.asType.toType
    c.Expr[IdMacros.IdType[Any]](Apply(
      Apply(
        TypeApply(
          Select(
            reify(IdMacros.IdType).tree, newTermName("apply")
          ),
          List(TypeTree(tpe))
        ),
        List()
      ),
      List(Select(reify(scala.Predef).tree, newTermName("implicitly")))
    ))
  }
  def g[A](id: Int)(implicit ev: IdType[A]) = {
    "foo".asInstanceOf[A]
  }

  private[this] val INT_TYPE_FILE = "iota-int-ids.txt"
  private[this] val STR_TYPE_FILE = "iota-str-ids.txt"
  private[this] var strMap = Map.empty[String,String]
  private[this] var intMap = Map.empty[Int,String]
  private[this] var intMapTime = 0l
  private[this] var strMapTime = 0l

  def tIdImpl[A <: View : c.WeakTypeTag](c: Context)(id: c.Expr[Int]): c.Expr[Kestrel[A]] = {
    import FileUtil._
    import c.universe._
    val tpeInfo = c.weakTypeOf[A].typeSymbol.fullName
    val idInfo: Either[String,Int] = id match {
      case c.Expr(Literal(Constant(n: Int))) => Right(n)
      case x => Left(x.tree.symbol.fullName)
    }

    val base = target(c.enclosingUnit.source.file.file)
    base.mkdirs()
    val strFile = file(base, STR_TYPE_FILE)
    val intFile = file(base, INT_TYPE_FILE)
    val (strs, ints) = loadMappings(strFile, intFile)
    val newMappingFile = idInfo.fold(_ => strFile, _ => intFile)
    def addMapping[M](mapping: Map[M,String], id: M): Unit = {
      val item = mapping.get(id)
      item match {
        case Some(tpe) =>
          if (tpe != tpeInfo) {
            val tpe1 = rootMirror.staticClass(tpe).baseClasses.collect { case c: ClassSymbol if !c.isTrait => c }.reverse
            val tpe2 = rootMirror.staticClass(tpeInfo).baseClasses.collect { case c: ClassSymbol if !c.isTrait => c }.reverse
            val common = (tpe1 zip tpe2).takeWhile(t => t._1 == t._2).lastOption map (_._1.fullName) getOrElse "android.view.View"
            c.warning(c.enclosingPosition, s"type reassigned, $tpe => $tpeInfo using $common")
            writeNewMapping(newMappingFile, common, idInfo)
          }
        case None =>
          writeNewMapping(newMappingFile, tpeInfo, idInfo)
      }
    }
    idInfo.right.foreach { addMapping(ints, _) }
    idInfo.left.foreach { addMapping(strs, _) }

    reify {
      kestrel ((v: A) => v.setId(id.splice))
    }
  }

  private[this] def writeNewMapping(f: java.io.File, tpe: String, idInfo: Either[String,Int]): Unit = {
    val fout = new OutputStreamWriter(new FileOutputStream(f, true), "utf-8")
    fout.write(s"$tpe ${idInfo.fold(identity,identity)}\n")
    fout.close()
  }

  private[this] def fileToMap[A,B](f: java.io.File, fn: (String => (A,B))): Map[A,B] = {
    if (f.isFile) {
      val src = io.Source.fromFile(f)
      try {
        src.getLines.map(fn).toMap
      } finally {
        src.close()
      }
    } else {
      Map.empty[A,B]
    }
  }
  private[this] def loadMappings(strfile: java.io.File, intfile: java.io.File): (Map[String,String],Map[Int,String]) = {
    if (intfile.lastModified > intMapTime) {
      intMap = fileToMap(intfile, { s =>
        val parts = s.split(' ')
        (parts(1).toInt, parts(0))
      })
      intMapTime = intfile.lastModified
    }
    if (strfile.lastModified > strMapTime) {
      strMap = fileToMap(strfile, { s =>
        val parts = s.split(' ')
        (parts(1), parts(0))
      })
      strMapTime = strfile.lastModified
    }
    (strMap, intMap)
  }
}
