package iota.module

import android.content.Context
import android.content.res.TypedArray
import android.util.{AttributeSet, TypedValue}

/**
  * @author pfnguyen
  */
object Themes extends Themes
private[iota] trait Themes {
  /** resolve a single theme attribute */
  def resolveAttr[A](attr: Int)(f: TypedValue => A)(implicit ctx: Context): Option[A] = {
    val tv = new TypedValue
    val r = ctx.getTheme.resolveAttribute(attr, tv, true)
    if (r) {
      Option(f(tv))
    } else None
  }
  /** retrieve a set of styled theme attributes */
  def themeAttrs[A](styleable: Array[Int])(f: TypedArray => A)(implicit context: Context): A = {
    val themeAttrs = context.getTheme.obtainStyledAttributes(styleable)
    try {
      f(themeAttrs)
    } finally {
      themeAttrs.recycle()
    }
  }
  /** retrieve a styled attributeset */
  def customAttrs[A](attrs: AttributeSet, styleable: Array[Int])(f: TypedArray => A)(implicit context: Context): A = {
    val themeAttrs = context.obtainStyledAttributes(attrs, styleable)
    try {
      f(themeAttrs)
    } finally {
      themeAttrs.recycle()
    }
  }
}
