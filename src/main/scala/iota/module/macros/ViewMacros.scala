package iota.module.macros

import android.content.{Context => AndroidContext}
import android.view.{View, ViewGroup}
import iota.module.IO
import iota.module.macros.IOViewGroupMacro.LpTransformer

import scala.reflect.macros.Context

/**
  * @author pfnguyen
  */
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
      c.Expr[B](transformer.transform(IOViewGroupMacro.lpTypeOf(c)(weakTypeOf[A]).toType, body.tree))
    }
  }
  def cw[A <: ViewGroup : c.WeakTypeTag, B](c: Context)(body: c.Expr[B]): c.Expr[B] = {
    val cwh = new CwHelper[c.type](c)
    cwh.cw(body)
  }
}
