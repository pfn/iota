package iota

import android.content.Context
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
  final implicit def contextViewExtension(c: Context):      ContextViewExtensions        = ContextViewExtensions(c)
}
object ViewTree {
  trait LayoutConstraint[A <: ViewGroup] extends Any
  trait LayoutParamConstraint[A <: ViewGroup.LayoutParams] extends Any
  import android.widget._
  case class ContextViewExtensions(c: Context) extends AnyVal {
    def v[A <: View](args: Any*) = ???
  }
  case class ViewLayoutExtensions(v: View) extends AnyVal {
    def lp(args: Any*) = macro ViewTreeMacro.lp
  }
  case class ViewLinearLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[LinearLayout] {
    def linearLayoutGravity(value: Int): Unit = macro ViewTreeMacro.layoutParamField
    def weight(value: Float):            Unit = macro ViewTreeMacro.layoutParamField
  }
  case class ViewFrameLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[FrameLayout] {
    def frameLayoutGravity(value: Int): Unit = macro ViewTreeMacro.layoutParamField
  }
  case class ViewMarginLayoutExtensions(v: View) extends AnyVal with LayoutParamConstraint[ViewGroup.MarginLayoutParams] {
    // would like a margins(t,l,r,b) but macros don't support default/named args
    def marginLeft(value: Int):   Unit = macro ViewTreeMacro.layoutParamField
    def marginTop(value: Int):    Unit = macro ViewTreeMacro.layoutParamField
    def marginBottom(value: Int): Unit = macro ViewTreeMacro.layoutParamField
    def marginRight(value: Int):  Unit = macro ViewTreeMacro.layoutParamField
  }
  case class ViewRelativeLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[RelativeLayout] {
    def above(view: View):          Unit = macro ViewTreeMacro.relativeLayoutParamView
    def alignBaseLine(view: View):  Unit = macro ViewTreeMacro.relativeLayoutParamView
    def alignBottom(view: View):    Unit = macro ViewTreeMacro.relativeLayoutParamView
    @TargetApi(17)
    def alignEnd(view: View):       Unit = macro ViewTreeMacro.relativeLayoutParamView
    def alignLeft(view: View):      Unit = macro ViewTreeMacro.relativeLayoutParamView
    def alignParentBottom():        Unit = macro ViewTreeMacro.relativeLayoutUnary
    @TargetApi(17)
    def alignParentEnd():           Unit = macro ViewTreeMacro.relativeLayoutUnary
    def alignParentLeft():          Unit = macro ViewTreeMacro.relativeLayoutUnary
    def alignParentRight():         Unit = macro ViewTreeMacro.relativeLayoutUnary
    @TargetApi(17)
    def alignParentStart():         Unit = macro ViewTreeMacro.relativeLayoutUnary
    def alignParentTop():           Unit = macro ViewTreeMacro.relativeLayoutUnary
    def alignRight(view: View):     Unit = macro ViewTreeMacro.relativeLayoutParamView
    def alignWithParentIfMissing(): Unit = macro ViewTreeMacro.relativeLayoutAlignParent
    @TargetApi(17)
    def alignStart(view: View):     Unit = macro ViewTreeMacro.relativeLayoutParamView
    def alignTop(view: View):       Unit = macro ViewTreeMacro.relativeLayoutParamView
    def below(view: View):          Unit = macro ViewTreeMacro.relativeLayoutParamView
    def centerHorizontal():         Unit = macro ViewTreeMacro.relativeLayoutUnary
    def centerInParent():           Unit = macro ViewTreeMacro.relativeLayoutUnary
    def centerVertical():           Unit = macro ViewTreeMacro.relativeLayoutUnary
    @TargetApi(17)
    def endOf(view: View):          Unit = macro ViewTreeMacro.relativeLayoutParamView
    def leftOf(view: View):         Unit = macro ViewTreeMacro.relativeLayoutParamView
    def rightOf(view: View):        Unit = macro ViewTreeMacro.relativeLayoutParamView
    @TargetApi(17)
    def startOf(view: View):        Unit = macro ViewTreeMacro.relativeLayoutParamView
  }

  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
}


