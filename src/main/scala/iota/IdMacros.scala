package iota

import java.io.{FileOutputStream, OutputStreamWriter}

import android.view.View

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.reflect.macros.Context

@implicitNotFound("cannot use findView prior to source application of id(Int)")
case class ViewIdType[+A : ClassTag]()
@implicitNotFound("cannot use tag(id) prior to source application of tag(id,value)")
case class TagIdType[+A : ClassTag]()
/**
  * @author pfnguyen
  */
trait IdMacros {
  implicit def materializeIdType: ViewIdType[Any] = macro IdMacros.matIdType
  implicit def materializeTagType: TagIdType[Any] = macro IdMacros.matTagType
}
private[iota] object IdMacros {
  def matIdType(c: Context): c.Expr[ViewIdType[Any]] = {
    import FileUtil._
    import c.universe._

    val base = target(c.enclosingUnit.source.file.file)
    val strFile = file(base, STR_IDS_FILE)
    val intFile = file(base, INT_IDS_FILE)

    val (strs, ints) = loadMappings(idsHolder, strFile, intFile)

    // check `Product` to be source compatible with scala 2.10 and 2.11
    val mapped = c.enclosingImplicits.headOption.fold {
      c.abort(c.enclosingPosition, "materializeIdType is not being used properly for implicit resolution")
    } { impls =>
      val tree = impls match {
        case p: Product if p.productArity == 2 => p.productElement(1).asInstanceOf[c.Tree]
        case p: Product if p.productArity == 4 => p.productElement(3).asInstanceOf[c.Tree]
      }
      val idInfo: Either[String,Int] = tree.collect {
        case Apply(_, y :: _) => y
      }.head match {
        case Literal(Constant(n: Int)) => Right(n)
        case t => Left(t.symbol.fullName)
      }

      idInfo.left.map(strs.get).right.map(ints.get).fold(identity,identity) getOrElse
        c.abort(c.enclosingPosition,
          "Id used before declaring in id(_), cannot determine type, aborting")
    }

    val tpe = rootMirror.staticClass(mapped).asType.toType
    c.Expr[ViewIdType[Any]](Apply(
      Apply(
        TypeApply(
          Select(reify(ViewIdType).tree, newTermName("apply")),
          List(TypeTree(tpe))
        ),
        List()
      ),
      List(Select(reify(scala.Predef).tree, newTermName("implicitly")))
    ))
  }
  def matTagType(c: Context): c.Expr[TagIdType[Any]] = {
    import FileUtil._
    import c.universe._

    val base = target(c.enclosingUnit.source.file.file)
    val strFile = file(base, STR_TAG_FILE)
    val intFile = file(base, INT_TAG_FILE)

    val (strs, ints) = loadMappings(tagHolder, strFile, intFile)

    // check `Product` to be source compatible with scala 2.10 and 2.11
    val mapped = c.enclosingImplicits.headOption.fold {
      c.abort(c.enclosingPosition, "materializeTagType is not being used properly for implicit resolution")
    } { impls =>
      val tree = impls match {
        case p: Product if p.productArity == 2 => p.productElement(1).asInstanceOf[c.Tree]
        case p: Product if p.productArity == 4 => p.productElement(3).asInstanceOf[c.Tree]
      }
      val idInfo: Either[String,Int] = tree.collect {
        case Apply(_, y :: _) => y
      }.head match {
        case Literal(Constant(n: Int)) => Right(n)
        case t => Left(t.symbol.fullName)
      }
      println(s"YO: $idInfo <= $ints / $strs")

      idInfo.left.map(strs.get).right.map(ints.get).fold(identity,identity) getOrElse
        c.abort(c.enclosingPosition,
          "Tag used before declaring in tag(id,value), cannot determine type, aborting")
    }

    val tpe = rootMirror.staticClass(mapped).asType.toType
    c.Expr[TagIdType[Any]](Apply(
      Apply(
        TypeApply(
          Select(reify(TagIdType).tree, newTermName("apply")),
          List(TypeTree(tpe))
        ),
        List()
      ),
      List(Select(reify(scala.Predef).tree, newTermName("implicitly")))
    ))
  }

  private[this] val INT_IDS_FILE = "iota-int-ids.txt"
  private[this] val STR_IDS_FILE = "iota-str-ids.txt"
  private[this] val INT_TAG_FILE = "iota-int-tags.txt"
  private[this] val STR_TAG_FILE = "iota-str-tags.txt"
  private[this] var idStrs  = (Map.empty[String,String], 0l)
  private[this] var idInts  = (Map.empty[Int,String], 0l)
  private[this] var tagStrs = (Map.empty[String,String], 0l)
  private[this] var tagInts = (Map.empty[Int,String], 0l)
  val idsHolder = MapHolder(
    idInts._1,
    { (a,b) => idInts = (a,b); a },
    idInts._2,
    idStrs._1,
    { (a,b) => idStrs = (a,b); a },
    idStrs._2)
  val tagHolder = MapHolder(
    tagInts._1,
    { (a,b) => tagInts = (a,b); a },
    tagInts._2,
    tagStrs._1,
    { (a,b) => tagStrs = (a,b); a },
    tagStrs._2)

  def vIdImpl[A <: View : c.WeakTypeTag](c: Context)(id: c.Expr[Int]): c.Expr[Kestrel[A]] = {
    import FileUtil._
    import c.universe._
    val tpeInfo = c.weakTypeOf[A].typeSymbol.fullName
    if (tpeInfo == "iota.ViewCombinators.A")
      c.abort(c.enclosingPosition, "Unable to determine concrete type for A")
    val idInfo: Either[String,Int] = id match {
      case c.Expr(Literal(Constant(n: Int))) => Right(n)
      case x => Left(x.tree.symbol.fullName)
    }

    val base = target(c.enclosingUnit.source.file.file)
    base.mkdirs()
    val strFile = file(base, STR_IDS_FILE)
    val intFile = file(base, INT_IDS_FILE)
    val (strs, ints) = loadMappings(idsHolder, strFile, intFile)
    val newMappingFile = idInfo.fold(_ => strFile, _ => intFile)
    def addMapping[M](mapping: Map[M,String], id: M): Unit = {
      val item = mapping.get(id)
      item match {
        case Some(tpe) =>
          if (tpe != tpeInfo) {
            // reassigning types will cause the mapping file to grow without bound
            // when building incrementally  :-( fix how?
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
    /*
    import c.universe._
    idImpl(c, "android.view.View", "iota.ViewCombinators.A", STR_IDS_FILE, INT_IDS_FILE, idsHolder)(id,
      reify {
        kestrel ((v: A) => v.setId(id.splice))
      }
    )
    */
  }
  def tIdImpl[T : c.WeakTypeTag](c: Context)(id: c.Expr[Int], value: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._
    idImpl(c, "scala.Any", "iota.ViewTagger.T", STR_TAG_FILE, INT_TAG_FILE, tagHolder)(id,
      c.Expr[Unit](
        Apply(
          Select(c.prefix.tree.children.tail.head, newTermName("setTag")),
          id.tree :: value.tree :: Nil
        )
      )
    )
  }

  def idImpl[A : c.WeakTypeTag, B](c: Context,
                                   commonbase: String,
                                   placeholder: String,
                                   inStrFile: String,
                                   inIntFile: String,
                                   holder: MapHolder[_,_])
                                  (id: c.Expr[Int], result: c.Expr[B]): c.Expr[B] = {
    import FileUtil._
    import c.universe._
    val tpeInfo = c.weakTypeOf[A].typeSymbol.fullName
    if (tpeInfo == placeholder)
      c.abort(c.enclosingPosition, "Unable to determine concrete type")
    val idInfo: Either[String,Int] = id match {
      case c.Expr(Literal(Constant(n: Int))) => Right(n)
      case x => Left(x.tree.symbol.fullName)
    }
    println(s"$placeholder: Generating new mapping $idInfo => $tpeInfo")

    val base = target(c.enclosingUnit.source.file.file)
    base.mkdirs()
    val strFile = file(base, inStrFile)
    val intFile = file(base, inIntFile)
    val (strs, ints) = loadMappings(holder, strFile, intFile)
    val newMappingFile = idInfo.fold(_ => strFile, _ => intFile)
    def addMapping[M](mapping: Map[M,String], id: M): Unit = {
      val item = mapping.get(id)
      item match {
        case Some(tpe) =>
          if (tpe != tpeInfo) {
            // reassigning types will cause the mapping file to grow without bound
            // when building incrementally  :-( fix how?
            val tpe1 = rootMirror.staticClass(tpe).baseClasses.collect { case c: ClassSymbol if !c.isTrait => c }.reverse
            val tpe2 = rootMirror.staticClass(tpeInfo).baseClasses.collect { case c: ClassSymbol if !c.isTrait => c }.reverse
            val common = (tpe1 zip tpe2).takeWhile(t => t._1 == t._2).lastOption map (_._1.fullName) getOrElse commonbase
            c.warning(c.enclosingPosition, s"type reassigned, $tpe => $tpeInfo using $common")
            writeNewMapping(newMappingFile, common, idInfo)
          }
        case None =>
          writeNewMapping(newMappingFile, tpeInfo, idInfo)
      }
    }
    idInfo.right.foreach { addMapping(ints, _) }
    println(s"$placeholder: Writing mapping $idInfo")
    idInfo.left.foreach { addMapping(strs, _) }

    result
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
  case class MapHolder[A,B](oldIntMap: Map[Int,String],
                            intMap: (Map[Int,String],Long) => Map[Int,String],
                            intLast: Long,
                            oldStrMap: Map[String,String],
                            strMap: (Map[String,String],Long) => Map[String,String],
                            strLast: Long)
  private[this] def loadMappings(holder: MapHolder[_,_],
                                 strfile: java.io.File, intfile: java.io.File): (Map[String,String],Map[Int,String]) = {
    println(s"$intfile -- ${intfile.isFile} ${intfile.lastModified} > ${holder.intLast}")
    val intMap = if (intfile.lastModified > holder.intLast) {
      holder.intMap(fileToMap(intfile, { s =>
        val parts = s.split(' ')
        (parts(1).toInt, parts(0))
      }), intfile.lastModified)
    } else holder.oldIntMap
    val strMap = if (strfile.lastModified > holder.strLast) {
      holder.strMap(fileToMap(strfile, { s =>
        val parts = s.split(' ')
        (parts(1), parts(0))
      }), strfile.lastModified)
    } else holder.oldStrMap
    (strMap, intMap)
  }
}
