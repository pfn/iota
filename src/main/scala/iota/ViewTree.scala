package iota

import android.content.{Context => AndroidContext}
import android.view.{View, ViewGroup}
import android.annotation.TargetApi


/**
  * @author pfnguyen
  */
trait ViewTree[A <: ViewGroup] {
  val container: A

  // implicits to allow implementations to access DSL
  import ViewTree._
  final implicit def viewLayoutExtensions(v: View):         ViewLayoutExtensions         = ViewLayoutExtensions(v)
  final implicit def viewLinearLayoutExtensions(v: View):   ViewLinearLayoutExtensions   = ViewLinearLayoutExtensions(v)
  final implicit def viewFrameLayoutExtensions(v: View):    ViewFrameLayoutExtensions    = ViewFrameLayoutExtensions(v)
  final implicit def viewRelativeLayoutExtensions(v: View): ViewRelativeLayoutExtensions = ViewRelativeLayoutExtensions(v)
  final implicit def viewMarginLayoutExtensions(v: View):   ViewMarginLayoutExtensions   = ViewMarginLayoutExtensions(v)
}
object ViewTree {
  trait LayoutConstraint[A <: ViewGroup] extends Any
  trait LayoutParamConstraint[A <: ViewGroup.LayoutParams] extends Any
  import android.widget._
  case class ViewLayoutExtensions(v: View) extends AnyVal {
    def lp(args: Any*) = macro ViewTreeMacro.lp
  }
  case class ViewLinearLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[LinearLayout] {
    def linearLayoutGravity(value: Int) = macro ViewTreeMacro.layoutParamField
    def weight(value: Float) = macro ViewTreeMacro.layoutParamField
  }
  case class ViewFrameLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[FrameLayout] {
    def frameLayoutGravity(value: Int) = macro ViewTreeMacro.layoutParamField
  }
  case class ViewMarginLayoutExtensions(v: View) extends AnyVal with LayoutParamConstraint[ViewGroup.MarginLayoutParams] {
    // would like a margins(t,l,r,b) but macros don't support default/named args
    def marginLeft(value: Int)   = macro ViewTreeMacro.layoutParamField
    def marginTop(value: Int)    = macro ViewTreeMacro.layoutParamField
    def marginBottom(value: Int) = macro ViewTreeMacro.layoutParamField
    def marginRight(value: Int)  = macro ViewTreeMacro.layoutParamField
  }
  case class ViewRelativeLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[RelativeLayout] {
    def above(view: View)          = macro ViewTreeMacro.relativeLayoutParamView
    def alignBaseLine(view: View)  = macro ViewTreeMacro.relativeLayoutParamView
    def alignBottom(view: View)    = macro ViewTreeMacro.relativeLayoutParamView
    @TargetApi(17)
    def alignEnd(view: View)       = macro ViewTreeMacro.relativeLayoutParamView
    def alignLeft(view: View)      = macro ViewTreeMacro.relativeLayoutParamView
    def alignParentBottom()        = macro ViewTreeMacro.relativeLayoutUnary
    @TargetApi(17)
    def alignParentEnd()           = macro ViewTreeMacro.relativeLayoutUnary
    def alignParentLeft()          = macro ViewTreeMacro.relativeLayoutUnary
    def alignParentRight()         = macro ViewTreeMacro.relativeLayoutUnary
    @TargetApi(17)
    def alignParentStart()         = macro ViewTreeMacro.relativeLayoutUnary
    def alignParentTop()           = macro ViewTreeMacro.relativeLayoutUnary
    def alignRight(view: View)     = macro ViewTreeMacro.relativeLayoutParamView
    def alignWithParentIfMissing() = macro ViewTreeMacro.relativeLayoutAlignParent
    @TargetApi(17)
    def alignStart(view: View)     = macro ViewTreeMacro.relativeLayoutParamView
    def alignTop(view: View)       = macro ViewTreeMacro.relativeLayoutParamView
    def below(view: View)          = macro ViewTreeMacro.relativeLayoutParamView
    def centerHorizontal()         = macro ViewTreeMacro.relativeLayoutUnary
    def centerInParent()           = macro ViewTreeMacro.relativeLayoutUnary
    def centerVertical()           = macro ViewTreeMacro.relativeLayoutUnary
    @TargetApi(17)
    def endOf(view: View)          = macro ViewTreeMacro.relativeLayoutParamView
    def leftOf(view: View)         = macro ViewTreeMacro.relativeLayoutParamView
    def rightOf(view: View)        = macro ViewTreeMacro.relativeLayoutParamView
    @TargetApi(17)
    def startOf(view: View)        = macro ViewTreeMacro.relativeLayoutParamView
  }

  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
}


