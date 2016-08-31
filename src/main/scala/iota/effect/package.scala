package iota
import iota.module.{AutoK, Combinators, FutureCombinators, IO, LayoutCombinators, TernaryCondition, TernaryOps, ViewCombinators, Views}
import iota.module.macros.{IdMacros, ViewIdType}

import scala.reflect.ClassTag

/**
 * @author pfnguyen
 */

package object effect
  extends Combinators
    with ViewCombinators
    with AutoK
    with IdMacros
    with Views
    with TernaryOps
    with FutureCombinators
    with LayoutCombinators
    with MainComponents {
  type IO[A] = module.IO[A]
  type Kestrel[A] = module.Kestrel[A]
  val IO = module.IO
  val Id = module.Id

  implicit class WithTernaryOp(val b: Boolean) extends AnyVal {
    def ?[A](ifTrue: Kestrel[A]): TernaryCondition[A] = TernaryCondition(b, ifTrue)
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
  implicit class IOKleisli[-A,+B](val f: A => IO[B]) extends AnyVal {
    /** f andThen g */
    def >=>[C](g: B => IO[C]): A => IO[C] = { a: A =>
      f(a) >>= g
    }
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
