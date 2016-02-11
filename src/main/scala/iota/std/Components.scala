package iota.std

import android.content.Context
import iota._

import scala.reflect.ClassTag

object Kleisli {
  implicit class IOKleisli[-A,+B](val f: A => IO[B]) extends AnyVal {
    /** f andThen g */
    def >=>[C](g: B => IO[C]): A => IO[C] = { a: A =>
      f(a) >>= g
    }
  }
}
object Combinators extends Combinators
object AutoK extends AutoK
object Single extends Single
object FutureCombinators extends FutureCombinators
object ImageCombinators extends ImageCombinators
object LayoutCombinators extends LayoutCombinators
object TextCombinators extends TextCombinators
object ViewCombinators extends ViewCombinators
object ViewCombinatorExtras extends ViewCombinatorExtras
object Contexts extends Contexts
object Themes extends Themes
object Views extends Views with IdMacros {
  implicit class ViewFinder(val vg: android.view.ViewGroup) extends AnyVal {
    /** find a strongly-typed view.
      * will fail to compile if id(xxx) is not used prior in the source
      */
    @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
    def findView[A <: android.view.View : ViewIdType : ClassTag](id: Int): A = {
      val v = vg.findViewById(id).asInstanceOf[A]
      if (v == null) throw new NullPointerException(s"view $id not found")
      v
    }
    /** find a strongly-typed view.
      * will fail to compile if id(xxx) is not used prior in the source
      */
    @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
    def findViewOption[A <: android.view.View : ViewIdType : ClassTag](id: Int): Option[A] =
      Option(vg.findViewById(id).asInstanceOf[A])
  }
}
object Ternary extends TernaryOps {
  implicit class WithTernaryOp(val b: Boolean) extends AnyVal {
    /** ternary expression creator */
    def ?[A](ifTrue: Kestrel[A]): TernaryCondition[A] = TernaryCondition(b, ifTrue)
  }
}

object Configurations extends Configurations {
  implicit class Metrics(val size: Int) extends AnyVal {
    /** convert dp to pixel values */
    @inline final def dp(implicit ctx: Context): Int =
      (ctx.getResources.getDisplayMetrics.density * size).toInt
    /** convert sp to pixel values */
    @inline final def sp(implicit ctx: Context): Int =
      (ctx.getResources.getDisplayMetrics.scaledDensity * size).toInt
  }
}

