import iota.Compat210._
import blackbox.Context

/**
 * @author pfnguyen
 */
package iota {
private[iota] trait AllComponents
  extends Combinators
  with IdMacros
  with Views
  with TernaryOps
  with Contexts
  with Configurations
  with ViewCombinators
  with ImageCombinators
  with LayoutCombinators
  with TextCombinators
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

  implicit class Metrics(val size: Int) extends AnyVal {
    import android.content.{Context => AndroidContext}
    /** convert dp to pixel values */
    @inline def dp(implicit ctx: AndroidContext): Int =
      (ctx.getResources.getDisplayMetrics.density * size).toInt
    /** convert sp to pixel values */
    @inline def sp(implicit ctx: AndroidContext): Int =
      (ctx.getResources.getDisplayMetrics.scaledDensity * size).toInt
  }

  implicit class IOKleisli[-A,+B](val f: A => IO[B]) extends AnyVal {
    /** f andThen g */
    def >=>[C](g: B => IO[C]): A => IO[C] = { a: A =>
      f(a) >>= g
    }
  }

  implicit class ViewFinder(val vg: android.view.ViewGroup) extends AnyVal {
    def findView[A <: android.view.View](id: Int)(implicit evidence: ViewIdType[A]): A = {
      val v = vg.findViewById(id).asInstanceOf[A]
      if (v == null) throw new NullPointerException(s"view $id not found")
      v
    }
    def findViewOption[A <: android.view.View](id: Int)(implicit evidence: ViewIdType[A]): Option[A] =
      Option(vg.findViewById(id).asInstanceOf[A])
  }
}
