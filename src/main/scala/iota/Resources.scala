package iota

import android.content.Context

/**
 * @author pfnguyen
 */
object Resources {
  implicit class Metrics(val size: Int) extends AnyVal {
    @inline def dp(implicit ctx: Context): Int = (ctx.getResources.getDisplayMetrics.density * size).toInt
    @inline def sp(implicit ctx: Context): Int = (ctx.getResources.getDisplayMetrics.scaledDensity * size).toInt
  }
}
