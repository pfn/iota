package iota.module

import android.app.Activity
import android.content.{Context => AndroidContext}
import android.net.nsd.NsdManager
import android.telephony.TelephonyManager
import iota.module.macros.{ContextMacro, SystemService, ViewIdType}

import scala.reflect.ClassTag

/**
 * @author pfnguyen
 */
object Contexts extends Contexts {
  implicit class ViewMaker(val ctx: AndroidContext) extends AnyVal {
    /** create a view `A` without specifying the `Context` and `AttributeSet` parameters
      * e.g `ctx.make[TextView]` or `ctx.make[ProgressBar](android.R.attr.progressBarStyleSmall)`
      */
    def make[A <: android.view.View](args: Any*): A = macro ContextMacro.create[A]
    def make[A <: android.view.View]:             A = macro ContextMacro.create2[A]
  }
}
private[iota] trait Contexts {
  type HasContext = macros.HasContext
  type HasActivity = macros.HasActivity
  /** pull a context out of "thin air", checks for Activity, Fragment and WithContext */
  implicit def materializeContext: AndroidContext = macro ContextMacro.materializeContextImpl

  implicit def materializeActivity: Activity = macro ContextMacro.materializeActivityImpl

  /** find a strongly-typed view.
    * will fail to compile if id(xxx) is not used prior in the source
    */
  @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
  def findView[A <: android.view.View : ViewIdType : ClassTag](id: Int)(implicit activity: Activity): A = {
    val v = activity.findViewById(id).asInstanceOf[A]
    if (v == null) throw new NullPointerException(s"view $id not found")
    v
  }

  /** find a strongly-typed view.
    * will fail to compile if id(xxx) is not used prior in the source
    */
  @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
  def findViewOption[A <: android.view.View : ViewIdType : ClassTag](id: Int)(implicit activity: Activity): Option[A] =
    Option(activity.findViewById(id).asInstanceOf[A])

  implicit val `nsd system service` =
    SystemService[NsdManager](AndroidContext.NSD_SERVICE)
  implicit val `telephony system service` =
    SystemService[TelephonyManager](AndroidContext.TELEPHONY_SERVICE)

  implicit def materializeSystemService[T]: SystemService[T] = macro ContextMacro.materializeSystemServiceImpl[T]
  /** type-safe retrieval of system service objects.
    * e.g. `systemService[NotificationManager]`
    */
  @inline final def systemService[T](implicit s: SystemService[T], context: AndroidContext): T =
    context.getSystemService(s.name).asInstanceOf[T]
}



