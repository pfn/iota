package iota.module

import android.content.{Context => AndroidContext}
import android.view.{View, ViewGroup}
import iota.module.macros.{IdMacros, ViewIdType, ViewMacros}

import scala.reflect.ClassTag

/**
  * @author pfnguyen
  */
object Views extends Views with IdMacros {
  implicit class ViewFinder(val vg: android.view.ViewGroup) extends AnyVal {
    /** find a strongly-typed view.
      * will fail to compile if id(xxx) is not used prior in the source
      */
    @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
    def findView[A <: android.view.View : ViewIdType : ClassTag](id: Int): A = {
      val v = vg.findViewById(id).asInstanceOf[A]
      if (v == null) throw new NullPointerException(s"view $id not found")
      v
    }
    /** find a strongly-typed view.
      * will fail to compile if id(xxx) is not used prior in the source
      */
    @deprecated("Use the view holder pattern for better compile-time safety", "0.9.2")
    def findViewOption[A <: android.view.View : ViewIdType : ClassTag](id: Int): Option[A] =
    Option(vg.findViewById(id).asInstanceOf[A])
  }
}
private[iota] trait Views {

  /** create any android `ViewGroup` that takes `Context` as a single constructor parameter.
    * additionally, provides type hints for the use of `lp` and `lpK` for constructing
    * `LayoutParams`
    */
  def l[A <: ViewGroup](vs: IO[_ <: View]*)(implicit ctx: AndroidContext): IO[A] = macro ViewMacros.l[A]
  /** create any android object that takes `Context` as a single constructor parameter */
  def w[A](implicit ctx: AndroidContext): IO[A] = macro ViewMacros.w[A]

  /** type inference currying helper for `c` */
  class cHelper[A <: ViewGroup] {
    def apply[B](body: B): B = macro ViewMacros.cw[A,B]
  }
  private[this] val chelper = new cHelper[ViewGroup]
  /** a type-hint is required when using `lp` or `lpK` outside of `IO[ViewGroup].apply(IO[View]*)`,
    * use `c[ViewGroup](B) => B` to provide `B` with the the type hint required to use `lp` and `lpK`
    */
  def c[A <: ViewGroup] = chelper.asInstanceOf[cHelper[A]] // prevent additional allocations
}


