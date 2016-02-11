package iota

import android.content.Context
import android.content.res.Configuration
import android.graphics.Point
import android.os.Build
import android.view.WindowManager

/**
 * @author pfnguyen
 */
private[iota] trait Configurations {

  private[this] lazy val displaySize = new Point(-1, -1)
  private[this] def getDisplaySize(implicit c: Context) = {
    if (displaySize.x == -1) {
      val d = c.getSystemService(
        Context.WINDOW_SERVICE).asInstanceOf[WindowManager].getDefaultDisplay
      if (Build.VERSION.SDK_INT >= 17) {
        d.getRealSize(displaySize)
      } else if (Build.VERSION.SDK_INT >= 14) {
        type RawSizeHack = {
          def getRawWidth: Int
          def getRawHeight: Int
        }
        val d2 = d.asInstanceOf[RawSizeHack]
        displaySize.x = d2.getRawWidth
        displaySize.y = d2.getRawHeight
      } else {
        displaySize.x = d.getWidth
        displaySize.y = d.getHeight
      }
    }
    displaySize
  }

  /** smallest width checker */
  @inline final def sw(widthPx: Int)(implicit c: Context) = {
    val p = getDisplaySize
    widthPx <= p.x && widthPx <= p.y
  }

  /** version checker, at least `minVersion` => true */
  @inline final def v(minVersion: Int) = Build.VERSION.SDK_INT >= minVersion

  /** orientation checker, true if landscape */
  @inline final def landscape(implicit c: Context) =
    c.getResources.getConfiguration.orientation == Configuration.ORIENTATION_LANDSCAPE
  /** orientation checker, true if portrait */
  @inline final def portrait(implicit c: Context) =
    c.getResources.getConfiguration.orientation == Configuration.ORIENTATION_PORTRAIT
}


