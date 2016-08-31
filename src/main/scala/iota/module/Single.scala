package iota.module

import iota.module.macros.SingleMacro

/**
  * @author pfnguyen
  */
object Single extends Single
private[iota] trait Single {
  import language.dynamics
  class single0Helper[A] extends Dynamic {
    def apply(body: Any): A = macro SingleMacro.apply0[A]
    def applyDynamic(method: String)(body: Any): A = macro SingleMacro.apply0Method[A]
  }
  class singleHelper[A] extends Dynamic {
    def apply(body: Any): A = macro SingleMacro.apply[A]
    def applyDynamic(method: String)(body: Any): A = macro SingleMacro.applyM[A]
  }
  private val singlehelper = new singleHelper[AnyRef]
  private val single0helper = new single0Helper[AnyRef]

  /**
    * Constructs a single instance of a given abstract class or interface,
    * can optionally specify the method name
    * example usage:
    * ```
    * view.setOnClickListener(single[View.OnClickListener] { v: View =>
    *   Toast.makeText(view.getContext, view.getId + " was clicked", Toast.LENGTH_SHORT).show()
    * })
    * ```
    *
    * this version, `single`, requires all input parameters to be specified
    * and handled
    */
  def single[A] = singlehelper.asInstanceOf[singleHelper[A]] // prevent additional allocations
  /**
    * Constructs a single instance of a given abstract class or interface,
    * can optionally specify the method name if multiple methods
    * need implementation, only the specified method will be implemented, and
    * others will have default, 0, implementations created
    *
    * example usage:
    * ```
    * view.animate().x(100).setListener(
    *   single0[AnimatorListener].onAnimationEnd(view.setVisibility(View.GONE)))
    * ```
    *
    * this version, `single0`, ignores all input parameters of the interface
    */
  def single0[A] = single0helper.asInstanceOf[single0Helper[A]]
}
