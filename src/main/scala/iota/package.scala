import iota.Compat210._
import blackbox.Context

import scala.reflect.ClassTag

/**
 * @author pfnguyen
 */
package iota {
private[iota] trait AllComponents
  extends Combinators
  with AutoK
  with Single
  with IdMacros
  with Views
  with TernaryOps
  with Contexts
  with Themes
  with Configurations
  with FutureCombinators
  with ViewCombinators
  with LayoutCombinators
}
package object iota extends AllComponents {
  type Kestrel[A] = A => IO[A]

  implicit class WithTernaryOp(val b: Boolean) extends AnyVal {
    def ?[A](ifTrue: Kestrel[A]): TernaryCondition[A] = TernaryCondition(b, ifTrue)
  }

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
    Apply(Select(reify(iota.Splicer).tree, newTermName("changeOwner")), t :: Nil)
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

  implicit class IOKleisli[-A,+B](val f: A => IO[B]) extends AnyVal {
    /** f andThen g */
    def >=>[C](g: B => IO[C]): A => IO[C] = { a: A =>
      f(a) >>= g
    }
  }

  implicit class ViewFinder(val vg: android.view.ViewGroup) extends AnyVal {
    /** will fail to compile if id(xxx) is not used prior in the source */
    @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
    @inline final def findView[A <: android.view.View : ViewIdType : ClassTag](id: Int): A = {
      val v = vg.findViewById(id).asInstanceOf[A]
      if (v == null) throw new NullPointerException(s"view $id not found")
      v
    }
    /** will fail to compile if id(xxx) is not used prior in the source */
    @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
    @inline final def findViewOption[A <: android.view.View : ViewIdType : ClassTag](id: Int): Option[A] =
      Option(vg.findViewById(id).asInstanceOf[A])
  }
}
