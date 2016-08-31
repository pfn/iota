import android.view.ViewGroup
import iota.module.macros.ContextMacro

/**
 * @author pfnguyen
 */
package iota {

  import iota.module.{Configurations, Contexts, DefaultExtensions, Single, Themes}

  private[iota] trait AllComponents
    extends Single
      with Contexts
      with Themes
      with Configurations
      with DefaultExtensions
}
package object iota extends AllComponents {
  type ViewTree[A <: ViewGroup] = module.ViewTree[A]
  val ViewTree = module.ViewTree

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
