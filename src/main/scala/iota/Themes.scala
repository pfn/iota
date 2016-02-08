package iota

import android.app.Activity
import android.content.Context
import android.content.res.TypedArray
import android.util.TypedValue

/**
  * @author pfnguyen
  */
private[iota] trait Themes {
  /** resolve a single theme attribute */
  def resolveAttr[A](attr: Int, f: TypedValue => A)(implicit ctx: Context): A = {
    val tv = new TypedValue
    val r = ctx.getTheme.resolveAttribute(attr, tv, true)
    if (r) {
      f(tv)
    } else throw new IllegalStateException("attribute not found: " + attr)
  }
  /** retrieve a set of styleable attributes */
  def styleableAttrs[A](styleable: Array[Int], f: TypedArray => A)(implicit context: Context): A = {
    val themeAttrs = context.getTheme.obtainStyledAttributes(styleable)
    val c = f(themeAttrs)
    themeAttrs.recycle()
    c
  }
}
