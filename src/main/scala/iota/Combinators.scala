package iota

import android.graphics.Color
import android.graphics.drawable.Drawable
import android.text.Html
import android.text.method.TransformationMethod
import android.view.{ViewGroup, View}
import android.widget.{ImageView, TextView}

import scala.reflect.internal.annotations.compileTimeOnly

/**
  * @author pfnguyen
  */

private[iota] trait Combinators {
  /** K-combinator, perform side-effects on A and automatically return A */
  def kestrel[A, B](f: A => B): Kestrel[A] = a => IO {
    f(a); a
  }

  private[iota] def noopK[A]: Kestrel[A] = kestrel(_ => ())
}
private[iota] trait ViewCombinators {
  def id[A <: View](id: Int): Kestrel[A] = macro IdMacros.tIdImpl[A]

  def visibility[A <: View](visibility: Int): Kestrel[A] =
    kestrel(_.setVisibility(visibility))
  def gone[A <: View]:      Kestrel[A] = visibility(View.GONE)
  def visible[A <: View]:   Kestrel[A] = visibility(View.VISIBLE)
  def invisible[A <: View]: Kestrel[A] = visibility(View.INVISIBLE)

  def enabled[A <: View](enable: Boolean): Kestrel[A] =
    kestrel(_.setEnabled(enable))
  def enabled[A <: View]:  Kestrel[A] = enabled(true)
  def disabled[A <: View]: Kestrel[A] = enabled(false)

  def elevation[A <: View](elevation: Float): Kestrel[A] = kestrel(_.setElevation(elevation))

  def padding[A <: View](left:   Int = 0,
                         top:    Int = 0,
                         right:  Int = 0,
                         bottom: Int = 0,
                         all: Int = -1): Kestrel[A] = kestrel { v =>
    if (all != -1) {
      v.setPadding(all, all, all, all)
    } else {
      v.setPadding(left, top, right, bottom)
    }
  }

  def clickable[A <: View](canclick: Boolean): Kestrel[A] = kestrel(_.setClickable(canclick))
  def clickable[A <: View]: Kestrel[A] = clickable[A](true)
  def backgroundDrawable[A <: View](d: Drawable): Kestrel[A] = kestrel(_.setBackgroundDrawable(d))
  def backgroundResource[A <: View](resid: Int): Kestrel[A] = kestrel(_.setBackgroundResource(resid))
  def backgroundColor[A <: View](color: Int):    Kestrel[A] = kestrel(_.setBackgroundColor(color))
  def backgroundColor[A <: View](color: String): Kestrel[A] = kestrel(_.setBackgroundColor(Color.parseColor(color)))

  import language.dynamics

  /** dynamic class for automatically setting event listeners. For example
    * `hook0.onClick(IO { perform work onClick })`, apply this to an
    * `IO[View]` using `>>=`
    */
  object hook0 extends Dynamic {
    def applyDynamic[V](event: String)(handler: IO[Any]): Kestrel[V] = macro HookMacro.applyHook0[V]
  }
  /** dynamic class for automatically setting event listeners. For example
    * `hookM0.onScroll.onScrollStateChanged(IO { perform work onClick })`, apply this to an
    * `IO[View]` using `>>=`
    */
  object hookM0 extends Dynamic {
    class _hookM0[V](val event: String) extends Dynamic {
      def applyDynamic(event: String)(handler: IO[Any]): Kestrel[V] = macro HookMacro.applyHookM0[V]
    }
    def selectDynamic[V](event: String): _hookM0[V] = new _hookM0[V](event)
  }
  /** dynamic class for automatically setting event listeners. For example
    * `hook.onClick((v: View) => IO { perform work onClick })`, apply this to an
    * `IO[View]` using `>>=`
    */
  object hook extends Dynamic {
    // must be Any because handler function may have arbitrary arity
    def applyDynamic[V](event: String)(handler: Any): Kestrel[V] = macro HookMacro.applyHook[V]
  }
  /** dynamic class for automatically setting event listeners. For example
    * `hookM.onScroll.onScrollStateChanged((rv: RecyclerView, state: Int) => IO { perform work onClick })`, apply this to an
    * `IO[View]` using `>>=`
    */
  object hookM extends Dynamic {
    class _hookM[V](val event: String) extends Dynamic {
      def applyDynamic(event: String)(handler: Any): Kestrel[V] = macro HookMacro.applyHookM[V]
    }
    def selectDynamic[V](event: String): _hookM[V] = new _hookM(event)
  }
}

private[iota] trait LayoutCombinators {

  @inline
  /** set margins on a `LayoutParams`,
    *  e.g. `lpK(MATCH_PARENT, MATCH_PARENT)(margins(all = 4.dp))`
    */
  def margins(left:   Int = 0,
              top:    Int = 0,
              right:  Int = 0,
              bottom: Int = 0,
              all: Int = -1): ViewGroup.MarginLayoutParams => Unit = lp => {
    if (all != -1) {
      lp.topMargin    = all
      lp.leftMargin   = all
      lp.rightMargin  = all
      lp.bottomMargin = all
    } else {
      lp.topMargin    = top
      lp.leftMargin   = left
      lp.rightMargin  = right
      lp.bottomMargin = bottom
    }
  }

  /** Set layout params for a view
    * `lp(MATCH_PARENT, MATCH_PARENT)`
    */
  @compileTimeOnly("lp can only be used from IO[_ <: ViewGroup].apply() or c[_ <: ViewGroup]()")
  def lp[V <: View](args: Any*): Kestrel[V] = ???

  @compileTimeOnly("lpK can only be used from IO[_ <: ViewGroup].apply() or c[_ <: ViewGroup]()")
  /** K-combinator for LayoutParams, e.g.
    *  ```
    *  lpK(MATCH_PARENT, MATCH_PARENT) { p: FrameLayout.LayoutParams =>
    *    p.marginTop = 10.dp
    *  }
    *  ``` */
  def lpK[V <: View,A,B](args: Any*)(k: A => B): Kestrel[V] = ???
}

private[iota] trait TextCombinators {
  def text[A <: TextView](text: CharSequence): Kestrel[A] = kestrel(_.setText(text))
  def text[A <: TextView](text: Int):          Kestrel[A] = kestrel(_.setText(text))

  def hint[A <: TextView](hint: CharSequence): Kestrel[A] = kestrel(_.setHint(hint))
  def hint[A <: TextView](hint: Int):          Kestrel[A] = kestrel(_.setHint(hint))

  def html[A <: TextView](html: String): Kestrel[A] = kestrel(_.setText(Html.fromHtml(html)))
  def hintColor[A <: TextView](color: Int): Kestrel[A] = kestrel(_.setHintTextColor(color))
  def textColor[A <: TextView](color: Int): Kestrel[A] = kestrel(_.setTextColor(color))
  def textTransformation[A <: TextView](transform: TransformationMethod): Kestrel[A] = kestrel(_.setTransformationMethod(transform))
  def inputType[A <: TextView](types: Int): Kestrel[A] = kestrel(_.setInputType(types))
  def textGravity[A <: TextView](gravity: Int): Kestrel[A] = kestrel(_.setGravity(gravity))
  def singleLine[A <: TextView](single: Boolean): Kestrel[A] = kestrel(_.setSingleLine(single))
  def singleLine[A <: TextView]: Kestrel[A] = singleLine[A](true)

  def textAppearance[A <: TextView](resid: Int): Kestrel[A] = kestrel(tv => tv.setTextAppearance(tv.getContext, resid))
}

private[iota] trait ImageCombinators {
  def imageResource[A <: ImageView](resid: Int): Kestrel[A] = kestrel(_.setImageResource(resid))
  def imageScale[A <: ImageView](scaleType: ImageView.ScaleType): Kestrel[A] = kestrel(_.setScaleType(scaleType))
}

