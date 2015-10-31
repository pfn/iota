package iota

import android.content.{Context => AndroidContext}
import scala.reflect.macros.{Context => MacroContext}

import language.experimental.macros

/**
 * @author pfnguyen
 */
object Contexts {
  implicit def materializeContext: AndroidContext = macro materializeContextImpl

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
      } else if (tpe <:< c.weakTypeOf[WithContext]) {
        Option(Select(This(tpe.typeSymbol), newTermName("getContext")))
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

}

trait WithContext {
  def getContext: AndroidContext
}
