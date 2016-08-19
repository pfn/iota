package iota

import android.content.Context
import android.view.{View, ViewGroup}
import android.annotation.TargetApi


/**
  * @author pfnguyen
  */
trait ViewTree[A <: ViewGroup] extends Iterable[ViewTree.Children] with Product {
  import ViewTree._

  /** @return an iterator with breadth-first traversal */
  override def iterator = new Iterator[Children] {
    var queue = collection.immutable.Queue[Children](Left(ViewTree.this))
    override def hasNext = queue.nonEmpty
    override def next() = {
      val (n, q) = queue.dequeue
      queue = q ++ (n match {
        case Left(vt) => vt.productIterator.map {
          case tree: ViewTree[_] => Left(tree)
          case view: View        => Right(view)
        }
        case Right(ch) => Nil
      })
      n
    }
  }

  val container: A

  // implicits to allow implementations to access DSL
  final implicit def viewLayoutExtensions(v: View):         ViewLayoutExtensions         = ViewLayoutExtensions(v)
  final implicit def viewLinearLayoutExtensions(v: View):   ViewLinearLayoutExtensions   = ViewLinearLayoutExtensions(v)
  final implicit def viewRelativeLayoutExtensions(v: View): ViewRelativeLayoutExtensions = ViewRelativeLayoutExtensions(v)
  final implicit def viewMarginLayoutExtensions(v: View):   ViewMarginLayoutExtensions   = ViewMarginLayoutExtensions(v)
  final implicit def viewGravityLayoutExtensions(v: View):  ViewGravityLayoutExtensions  = ViewGravityLayoutExtensions(v)
}
object ViewTree {
  type Children = Either[ViewTree[_], _ <: View]
  trait LayoutConstraint[A <: ViewGroup] extends Any
  /** a second layout type constraint, `OR`d with any other `LayoutConstraint` */
  trait LayoutConstraint2[A <: ViewGroup] extends Any
  trait LayoutParamConstraint[A <: ViewGroup.LayoutParams] extends Any
  import android.widget._
  case class ViewLayoutExtensions(v: View) extends AnyVal {
    /** construct LayoutParams for the correct type with the specified arguments.
      * If `lp` is not called, any layout decorator call will insert default
      * LayoutParams. Default LayoutParams have WRAP_CONTENT for both width and height
      */
    def lp(args: Any*) = macro ViewTreeMacro.lp
  }
  case class ViewGravityLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[LinearLayout] with LayoutConstraint2[FrameLayout] {
    def gravity(value: Int): Unit = macro ViewTreeMacro.layoutParamField
  }
  case class ViewLinearLayoutExtensions(v: View) extends AnyVal with LayoutConstraint[LinearLayout] {
    def weight(value: Float):            Unit = macro ViewTreeMacro.layoutParamField
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


