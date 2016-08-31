package iota.module

/**
 * @author pfnguyen
 */
import iota.module.macros.Compat210._
import blackbox.Context

package object macros {
  type Kestrel[A] = A => IO[A]

  private[iota] def sequence[A](c: Context)(xs: Seq[c.Expr[A]]): c.Expr[Seq[A]] = {
    import c.universe._
    c.Expr(Apply(Select(reify(Seq).tree, newTermName("apply")),
      xs.map(_.tree).toList
    ))
  }
  private[iota] def spliceTree(c: Context)(enclosingOwner: c.Symbol, t: c.Tree): c.Tree = {
    import c.universe._
    //     smuggle the symbol of the current enclosing owner through to the
    //     `changeOwner` as the symbol of the tree of its first argument.
    //     Tree attachments would be a more principled approach, but they aren't
    //     part of the public API.
    import internal._, decorators._
    t.updateAttachment(OrigOwnerAttachment(enclosingOwner))
    Apply(Select(reify(Splicer).tree, newTermName("changeOwner")), t :: Nil)
  }

  implicit class ViewMaker(val ctx: android.content.Context) extends AnyVal {
    /** create a view `A` without specifying the `Context` and `AttributeSet` parameters
      * e.g `ctx.make[TextView]` or `ctx.make[ProgressBar](android.R.attr.progressBarStyleSmall)`
      */
    def make[A <: android.view.View](args: Any*): A = macro ContextMacro.create[A]
    def make[A <: android.view.View]:             A = macro ContextMacro.create2[A]
  }

  implicit class Metrics(val size: Int) extends AnyVal {
    import android.content.{Context => AndroidContext}
    /** convert dp to pixel values */
    @inline final def dp(implicit ctx: AndroidContext): Int =
    (ctx.getResources.getDisplayMetrics.density * size).toInt
    /** convert sp to pixel values */
    @inline final def sp(implicit ctx: AndroidContext): Int =
    (ctx.getResources.getDisplayMetrics.scaledDensity * size).toInt
  }

}
