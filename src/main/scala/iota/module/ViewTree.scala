package iota.module

import android.annotation.TargetApi
import android.view.{View, ViewGroup}
import android.widget._
import iota.module.macros.ViewTreeMacro

/** a trait for creating fully type-safe view hierarchies
  * {{{
  *     lazy val views = ViewTree.inflateF(this, Main) {
  *        case "progress.pb1" =>
  *            this.make[ProgressBar](android.R.attr.progressBarStyleSmall)
  *        case "progress.pb2" =>
  *            this.make[ProgressBar](android.R.attr.progressBarStyleHorizontal)
  *    }
  *    case class ProgressBars(
  *        container: LinearLayout,
  *        pb1: ProgressBar,
  *        pb2: ProgressBar,
  *        pb3: ProgressBar
  *    ) extends ViewTree[LinearLayout] {
  *      ...
  *    }
  *    case class Main(
  *        container: LinearLayout,
  *        text: TextView,
  *        image: ImageView,
  *        progress: ProgressBars,
  *        ok: Button,
  *        cancel: Button
  *    ) extends ViewTree[LinearLayout] {
  *      ...
  *    }
  * }}}
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

  /** create an anonymous viewgroup `B`, allows inserting a level in the view hierarchy without
    * having to explicitly nest ViewTree classes, `views` will be added to the new `B`
    * automatically. Anything in `{ body }` will be executed in the context of `B`, such as layout
    * parameter settings
    */
  def nest[B <: ViewGroup](views: View*)(body: Any): B = macro ViewTreeMacro.nest[B]

  /** create an anonymous view `B` without specifying the `Context` and `AttributeSet` parameters
    * e.g `create[TextView]` or `create[ProgressBar](android.R.attr.progressBarStyleSmall)`
    */
  def create[B <: View](args: Any*): B = macro ViewTreeMacro.create[B]
  def create[B <: View]:             B = macro ViewTreeMacro.create2[B]

  val container: A

  // implicits to allow implementations to access DSL
  final implicit def viewLayoutExtensions        [Z <: View](v: Z): ViewLayoutExtensions[Z]         = ViewLayoutExtensions(v)
  final implicit def viewLinearLayoutExtensions  [Z <: View](v: Z): ViewLinearLayoutExtensions[Z]   = ViewLinearLayoutExtensions(v)
  final implicit def viewRelativeLayoutExtensions[Z <: View](v: Z): ViewRelativeLayoutExtensions[Z] = ViewRelativeLayoutExtensions(v)
  final implicit def viewMarginLayoutExtensions  [Z <: View](v: Z): ViewMarginLayoutExtensions[Z]   = ViewMarginLayoutExtensions(v)
  final implicit def viewGridLayoutExtensions    [Z <: View](v: Z): ViewGridLayoutExtensions[Z]     = ViewGridLayoutExtensions(v)
  final implicit def viewGravityLayoutExtensions [Z <: View](v: Z): ViewGravityLayoutExtensions[Z]  = ViewGravityLayoutExtensions(v)
}
object ViewTree extends ViewTreeBoilerplate.Inflate
  with ViewTreeBoilerplate.InflateF with ViewTreeBoilerplate.Check {

  /** an annotation to suppress warnings generated by `inflateF` */
  class UnsafeOperation extends annotation.StaticAnnotation

  type Children = Either[ViewTree[_], _ <: View]

  /** describes which viewgroup can use a contained layout param decorator */
  trait LayoutConstraint[A[_ <: View], -B <: ViewGroup] extends Any
  object LayoutConstraint {
    implicit val gravityLinearLayoutConstraint: LayoutConstraint[ViewGravityLayoutExtensions,LinearLayout] = null
    implicit val gravityFrameLayoutConstraint: LayoutConstraint[ViewGravityLayoutExtensions,FrameLayout] = null
    implicit val linearLayoutWeightConstraints: LayoutConstraint[ViewLinearLayoutExtensions,LinearLayout] = null
    implicit val gridLayoutSpecConstraints: LayoutConstraint[ViewGridLayoutExtensions,GridLayout] = null
    implicit val relativeLayoutParamConstraints: LayoutConstraint[ViewRelativeLayoutExtensions,RelativeLayout] = null
    implicit val standardLayoutParamConstraints: LayoutConstraint[ViewLayoutExtensions,ViewGroup] = null
  }
  /** describes which layout params type can use a contained layout param decorator */
  trait LayoutParamConstraint[A <: ViewGroup.LayoutParams] extends Any

  /** generic layout params decorators */
  case class ViewLayoutExtensions[A <: View](v: A) extends AnyVal {
    /** construct LayoutParams for the correct type with the specified arguments.
      * If `lp` is not called, any layout decorator call will insert default
      * LayoutParams. Default LayoutParams have WRAP_CONTENT for both width and height
      */
    def lp         (args: Any*): A = macro ViewTreeMacro.lp[A]
    def matchWidth ():           A = macro ViewTreeMacro.layoutParamField2[A]
    def matchHeight():           A = macro ViewTreeMacro.layoutParamField2[A]
    def wrapWidth  ():           A = macro ViewTreeMacro.layoutParamField2[A]
    def wrapHeight ():           A = macro ViewTreeMacro.layoutParamField2[A]
  }
  /** decorators for containers that support gravity, `LinearLayout` and `FrameLayout` */
  case class ViewGravityLayoutExtensions[A <: View](v: A) extends AnyVal {
    def gravity(value: Int): A = macro ViewTreeMacro.layoutParamField[A]
  }
  /** layout param decorators for `LinearLayout`s */
  case class ViewLinearLayoutExtensions[A <: View](v: A) extends AnyVal {
    def weight(value: Float): A = macro ViewTreeMacro.layoutParamField[A]
  }
  /** layout param decorators for `GridLayout`s */
  case class ViewGridLayoutExtensions[A <: View](v: A) extends AnyVal {
    def rowSpec(value: GridLayout.Spec): A = macro ViewTreeMacro.layoutParamField[A]
    def colSpec(value: GridLayout.Spec): A = macro ViewTreeMacro.layoutParamField[A]
  }
  /** margin decorators for containers that support margins */
  case class ViewMarginLayoutExtensions[A <: View](v: A) extends AnyVal with LayoutParamConstraint[ViewGroup.MarginLayoutParams] {
    // would like a margins(t,l,r,b) but macros don't support default/named args
    def marginLeft  (value: Int): A = macro ViewTreeMacro.layoutParamField[A]
    def marginTop   (value: Int): A = macro ViewTreeMacro.layoutParamField[A]
    def marginBottom(value: Int): A = macro ViewTreeMacro.layoutParamField[A]
    def marginRight (value: Int): A = macro ViewTreeMacro.layoutParamField[A]
  }
  /** relativelayout param decorators */
  case class ViewRelativeLayoutExtensions[A <: View](v: View) extends AnyVal {
    def above                   (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def alignBaseLine           (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def alignBottom             (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    @TargetApi(17)
    def alignEnd                (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def alignLeft               (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def alignParentBottom       (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    @TargetApi(17)
    def alignParentEnd          (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    def alignParentLeft         (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    def alignParentRight        (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    @TargetApi(17)
    def alignParentStart        (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    def alignParentTop          (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    def alignRight              (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def alignWithParentIfMissing(): A = macro ViewTreeMacro.relativeLayoutAlignParent[A]
    @TargetApi(17)
    def alignStart              (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def alignTop                (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def below                   (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def centerHorizontal        (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    def centerInParent          (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    def centerVertical          (): A = macro ViewTreeMacro.relativeLayoutUnary[A]
    @TargetApi(17)
    def endOf                   (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def leftOf                  (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    def rightOf                 (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
    @TargetApi(17)
    def startOf                 (view: View): A = macro ViewTreeMacro.relativeLayoutParamView[A]
  }
}
