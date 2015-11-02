package iota.std

import android.content.Context
import iota._

object Kleisli {
  implicit class IOKleisli[-A,+B](val f: A => IO[B]) extends AnyVal {
    /** f andThen g */
    def >=>[C](g: B => IO[C]): A => IO[C] = { a: A =>
      f(a) >>= g
    }
  }
}
object Combinators extends Combinators
object ImageCombinators extends ImageCombinators
object LayoutCombinators extends LayoutCombinators
object TextCombinators extends TextCombinators
object ViewCombinators extends ViewCombinators
object Contexts extends Contexts
object Views extends Views
object Ternary extends TernaryOps {
  implicit class WithTernaryOp(val b: Boolean) extends AnyVal {
    /** ternary expression creator */
    def ?[A](ifTrue: Kestrel[A]): TernaryCondition[A] = TernaryCondition(b, ifTrue)
  }
}

object Configurations extends Configurations {
  implicit class Metrics(val size: Int) extends AnyVal {
    /** convert dp to pixel values */
    @inline def dp(implicit ctx: Context): Int =
      (ctx.getResources.getDisplayMetrics.density * size).toInt
    /** convert sp to pixel values */
    @inline def sp(implicit ctx: Context): Int =
      (ctx.getResources.getDisplayMetrics.scaledDensity * size).toInt
  }
}

