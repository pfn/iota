package iota

import android.content.{Context => AndroidContext}
import android.view.{ViewGroup, View}
import iota.IOViewGroupMacro.LpTransformer

import scala.reflect.macros.Context

/**
  * @author pfnguyen
  */
private[iota] trait Views {

  /** create any android `ViewGroup` that takes `Context` as a single constructor parameter.
    * additionally, provides type hints for the use of `lp` and `lpK` for constructing
    * `LayoutParams`
    */
  def l[A <: ViewGroup](vs: IO[_ <: View]*)(implicit ctx: AndroidContext): IO[A] = macro ViewMacros.l[A]
  /** create any android object that takes `Context` as a single constructor parameter */
  def w[A](implicit ctx: AndroidContext): IO[A] = macro ViewMacros.w[A]

  /** type inference currying helper for `c` */
  class cHelper[A <: ViewGroup] {
    def apply[B](body: B): B = macro ViewMacros.cw[A,B]
  }
  private[this] val chelper = new cHelper[ViewGroup]
  /** a type-hint is required when using `lp` or `lpK` outside of `IO[ViewGroup].apply(IO[View]*)`,
    * use `c[ViewGroup](B) => B` to provide `B` with the the type hint required to use `lp` and `lpK`
    */
  def c[A <: ViewGroup] = chelper.asInstanceOf[cHelper[A]] // prevent additional allocations
}

private[iota] object ViewMacros {
  def w[A : c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext]): c.Expr[IO[A]] = {
    import c.universe._
    val tp = weakTypeOf[A]
    val newA = c.Expr[A](Apply(
      Select(New(TypeTree(tp)), nme.CONSTRUCTOR), ctx.tree :: Nil))
    reify(IO(newA.splice))
  }
  // for some reason, can't just do w[ViewGroup].apply(), just throws weird Nothing errors,
  // so do l[ViewGroup](IO[View]*) instead
  def l[A <: ViewGroup : c.WeakTypeTag](c: Context)(vs: c.Expr[IO[_ <: View]]*)(ctx: c.Expr[AndroidContext]): c.Expr[IO[A]] = {
    import c.universe._
    val tp = weakTypeOf[A]
    val iovm = new IOViewGroupMacro.IOViewGroupMacro[c.type](c)
    val newA = c.Expr[A](Apply(
      Select(New(TypeTree(tp)), nme.CONSTRUCTOR), ctx.tree :: Nil))
    iovm.applyVGImpl(reify(IO(newA.splice)).tree, vs)
  }

  class CwHelper[C <: Context](val c: C) extends Internal210 {
    import c.universe._
    def cw[A <: ViewGroup : WeakTypeTag, B](body: Expr[B]): Expr[B] = {
      val transformer = new LpTransformer[c.type](c)
      c.Expr[B](transformer.transform(transformer.lpTypeOf(weakTypeOf[A]).toType, body.tree))
    }
  }
  def cw[A <: ViewGroup : c.WeakTypeTag, B](c: Context)(body: c.Expr[B]): c.Expr[B] = {
    val cwh = new CwHelper[c.type](c)
    cwh.cw(body)
  }
}
