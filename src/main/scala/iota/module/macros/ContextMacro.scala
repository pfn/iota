package iota.module.macros
import scala.reflect.macros.{Context => MacroContext}
import android.content.{Context => AndroidContext}
import android.app.Activity

import scala.annotation.implicitNotFound

/**
  * @author pfnguyen
  */
private[iota] object ContextMacro {
  private[this] val TYPES =
    "android.app.Context" ::
    "android.app.Fragment" ::
    "android.view.View" ::
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
      } else if (tpe <:< c.weakTypeOf[HasActivity]) {
        Option(Select(This(tpe.typeSymbol), newTermName("activity")))
      } else if (tpe <:< c.weakTypeOf[android.view.View]) {
        Option(Apply(Select(This(tpe.typeSymbol), newTermName("getContext")), Nil))
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

    if (service.isEmpty)
      c.abort(c.enclosingPosition, s"No service constant found for $tpe")
    c.Expr[SystemService[T]](Apply(TypeApply(
      Select(reify(iota.module.macros.SystemService).tree, newTermName("apply")),
      List(TypeTree(tpe))
    ), List(Literal(Constant(service)))))
  }


  def create[A <: android.view.View : c.WeakTypeTag](c: MacroContext)(args: c.Expr[Any]*): c.Expr[A] = {
    import c.universe._
    val t = weakTypeOf[A]
    val ctx = c.prefix.tree.children.last
    // TODO inspect ctors to see if it has necessary Context and AttributeSet params
    // ignore, skip, etc. if not present
    c.Expr(Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR),
      ctx :: Literal(Constant(null)) :: (args.toList.map(_.tree) ++ Nil)))
  }
  def create2[A <: android.view.View : c.WeakTypeTag](c: MacroContext): c.Expr[A] = create[A](c)()
}

/** When a `android.content.Context` can't be found automatically using
  * the implicits in `iota._` or `Contexts._` implement this trait
  * to help the implicit out
  */
trait HasContext {
  def context: AndroidContext
}

/** When an `android.app.Activity` can't be found automatically using
  * the implicits in `iota._` or `Contexts._` implement this trait
  * to help the implicit out
  */
trait HasActivity {
  def activity: Activity
}
@implicitNotFound("Unable to find a service constant for ${T},\n" +
  "add the following implicit value to your code if this is not a mistake:\n    " +
  "'implicit val `systemService for ${T}` =\n      " +
  "SystemService[${T}](GET_SERVICE_CONSTANT)`'")
case class SystemService[T](name: String) extends AnyVal
