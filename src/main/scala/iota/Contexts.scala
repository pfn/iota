package iota

import android.app.Activity
import android.content.{Context => AndroidContext}
import android.net.nsd.NsdManager
import android.telephony.TelephonyManager
import scala.reflect.ClassTag
import scala.reflect.macros.{Context => MacroContext}

/**
 * @author pfnguyen
 */
case class SystemService[T](name: String) extends AnyVal
private[iota] trait Contexts {
  /** pull a context out of "thin air", checks for Activity, Fragment and WithContext */
  implicit def materializeContext: AndroidContext = macro ContextMacro.materializeContextImpl

  implicit def materializeActivity: Activity = macro ContextMacro.materializeActivityImpl

  /** find a strongly-typed view.
    * will fail to compile if id(xxx) is not used prior in the source
    */
  def findView[A <: android.view.View : ViewIdType : ClassTag](id: Int)(implicit activity: Activity): A = {
    val v = activity.findViewById(id).asInstanceOf[A]
    if (v == null) throw new NullPointerException(s"view $id not found")
    v
  }

  /** find a strongly-typed view.
    * will fail to compile if id(xxx) is not used prior in the source
    */
  def findViewOption[A <: android.view.View : ViewIdType : ClassTag](id: Int)(implicit activity: Activity): Option[A] =
    Option(activity.findViewById(id).asInstanceOf[A])

  implicit val `nsd system service` =
    SystemService[NsdManager](AndroidContext.NSD_SERVICE)
  implicit val `telephony system service` =
    SystemService[TelephonyManager](AndroidContext.TELEPHONY_SERVICE)

  implicit def materializeSystemService[T]: SystemService[T] = macro ContextMacro.materializeSystemServiceImpl[T]
  /** type-safe retrieval of system service objects.
    * e.g. `systemService[NotificationManager]`
    */
  @inline def systemService[T](implicit s: SystemService[T], context: AndroidContext): T =
    context.getSystemService(s.name).asInstanceOf[T]
}
private[iota] object ContextMacro {
  private[this] val TYPES =
    "android.app.Context" ::
    "android.app.Fragment" ::
    "android.support.v4.app.Fragment" ::
    "iota.WithContext" ::
    Nil
  def materializeContextImpl(c: MacroContext): c.Expr[AndroidContext] = {
    import c.universe._
    val supportFragment = util.Try(rootMirror.staticClass("android.support.v4.app.Fragment")).toOption
    val classType = c.enclosingClass.symbol.asType.toType

    def dfs(body: c.Tree, pos: c.Position, path: List[c.Tree]): List[c.Tree] =
      if (body.pos == pos) path.collect { case a@ClassDef(_,_,_,_) => a }
      else body.children.flatMap { child => dfs(child, pos, body :: path) }

    def expandTree(tpe: Type): Option[c.Tree] = {
      if (tpe <:< c.weakTypeOf[AndroidContext]) {
        Option(This(tpe.typeSymbol))
      } else if (tpe <:< c.weakTypeOf[HasContext]) {
        Option(Select(This(tpe.typeSymbol), newTermName("context")))
      } else if (tpe <:< c.weakTypeOf[android.app.Fragment]) {
        Option(Apply(Select(This(tpe.typeSymbol), newTermName("getActivity")), Nil))
      } else if (supportFragment.exists(tpe <:< _.toType)) {
        Option(Apply(Select(This(tpe.typeSymbol), newTermName("getActivity")), Nil))
      } else {
        None
      }
    }

    def error = c.abort(c.enclosingPosition,
      s"$classType does not extend any of:\n    ${TYPES mkString "\n    "}")

    c.Expr(expandTree(classType).getOrElse {
      val paths = dfs(c.enclosingUnit.body, c.enclosingPosition, Nil)
      paths.find(_.symbol.asType.toType <:< c.weakTypeOf[AndroidContext]).fold(
        error
      )(t => expandTree(t.symbol.asType.toType).getOrElse (error))
    })
  }

  def materializeActivityImpl(c: MacroContext): c.Expr[Activity] = {
    import c.universe._
    val supportFragment = util.Try(rootMirror.staticClass("android.support.v4.app.Fragment")).toOption
    val classType = c.enclosingClass.symbol.asType.toType

    def dfs(body: c.Tree, pos: c.Position, path: List[c.Tree]): List[c.Tree] =
      if (body.pos == pos) path.collect { case a@ClassDef(_,_,_,_) => a }
      else body.children.flatMap { child => dfs(child, pos, body :: path) }

    def expandTree(tpe: Type): Option[c.Tree] = {
      if (tpe <:< c.weakTypeOf[Activity]) {
        Option(This(tpe.typeSymbol))
      } else if (tpe <:< c.weakTypeOf[HasActivity]) {
        Option(Select(This(tpe.typeSymbol), newTermName("activity")))
      } else if (tpe <:< c.weakTypeOf[android.app.Fragment]) {
        Option(Apply(Select(This(tpe.typeSymbol), newTermName("getActivity")), Nil))
      } else if (supportFragment.exists(tpe <:< _.toType)) {
        Option(Apply(Select(This(tpe.typeSymbol), newTermName("getActivity")), Nil))
      } else {
        None
      }
    }

    def error = c.abort(c.enclosingPosition,
      s"$classType does not extend any of:\n    ${TYPES mkString "\n    "}")

    c.Expr(expandTree(classType).getOrElse {
      val paths = dfs(c.enclosingUnit.body, c.enclosingPosition, Nil)
      paths.find(_.symbol.asType.toType <:< c.weakTypeOf[AndroidContext]).fold(
        error
      )(t => expandTree(t.symbol.asType.toType).getOrElse (error))
    })
  }

  private[this] lazy val SERVICE_CONSTANTS = {
    val fields = classOf[AndroidContext].getDeclaredFields filter {
      _.getName endsWith "_SERVICE"
    }
    fields map { f =>
      val v = f.get(null).toString
      v.replaceAll("_", "") -> v
    } toSeq
  }
  def materializeSystemServiceImpl[T: c.WeakTypeTag](c: MacroContext): c.Expr[SystemService[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val candidates = SERVICE_CONSTANTS filter (tpe.toString.toLowerCase contains _._1)
    val service = ("" /: candidates) { (a, b) =>
      if (a.length > b._2.length) a else b._2
    }

    c.Expr[SystemService[T]](Apply(TypeApply(
      Select(reify(iota.SystemService).tree, newTermName("apply")),
      List(TypeTree(tpe))
    ), List(Literal(Constant(service)))))
  }
}

/** When a `android.content.Context` can't be found automatically using
  * the implicits in `iota._` or `iota.std.Contexts._` implement this trait
  * to help the implicit out
  */
trait HasContext {
  def context: AndroidContext
}

/** When an `android.app.Activity` can't be found automatically using
  * the implicits in `iota._` or `iota.std.Contexts._` implement this trait
  * to help the implicit out
  */
trait HasActivity {
  def activity: Activity
}
