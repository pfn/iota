package iota

import scala.reflect.macros.Context

/**
  * @author pfnguyen
  */
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

private[iota] object SingleMacro {
  def apply0[A: c.WeakTypeTag](c: Context)(body: c.Expr[Any]): c.Expr[A] = {
    val helper = new SingleMacro[c.type](c)
    helper.applySingle0(body)
  }

  def apply0Method[A: c.WeakTypeTag](c: Context)(method: c.Expr[String])(body: c.Expr[Any]): c.Expr[A] = {
    val helper = new SingleMacro[c.type](c)
    helper.applySingleM0(method)(body)
  }

  def apply[A: c.WeakTypeTag](c: Context)(body: c.Expr[Any]): c.Expr[A] = {
    val helper = new SingleMacro[c.type](c)
    helper.apply(body)
  }

  def applyM[A: c.WeakTypeTag](c: Context)(method: c.Expr[String])(body: c.Expr[Any]): c.Expr[A] = {
    val helper = new SingleMacro[c.type](c)
    helper.applyM(method)(body)
  }

  private[iota] class SingleMacro[C <: Context](val c: C) extends ListenerMacros[C] {
    private val OBJECT_FUNCTIONS = Set("clone", "toString", "hashCode", "equals", "finalize")

    import c.universe._

    def needImplementation(listener: Type): List[MethodSymbol] = {
      if (!listener.typeSymbol.asClass.isAbstractClass) Nil
      else listener.members.collect {
        case m: MethodSymbol if !m.isFinal && m.isJava && !m.isConstructor && !OBJECT_FUNCTIONS(m.name.encoded) =>
          m
      }.toList
    }
    def callbackPartsFor[V: c.WeakTypeTag](e: Option[String]) = {
      val listener = weakTypeOf[V]

      // hack for partially implemented listeners, until we move to 2.11 only and can use isAbstract
      val toImplement = needImplementation(listener)

      val on = e.map { x =>
        val onMethod = x
        util.Try {
          listener.member(newTermName(onMethod)).asMethod
        }.recover { case _ =>
          listener.member(newTermName("on" + onMethod.capitalize)).asMethod
        } getOrElse {
          c.abort(c.enclosingPosition, s"Unsupported event listener (no method '$onMethod' in $listener)")
        }
      }.orElse(toImplement.headOption).getOrElse(
        c.abort(c.enclosingPosition, s"Cannot identify type to implement")
      )

      (listener, on, toImplement.toList filter (_.name.encoded != on.name.encoded))
    }

    def applySingle0[A: c.WeakTypeTag](body: c.Expr[Any]): c.Expr[A] = {
      val (listener, on, toOverride) = callbackPartsFor(None)
      c.Expr(newListenerClass(listener, on, body.tree, toOverride, false, false))
    }

    def applySingleM0[A: c.WeakTypeTag](method: c.Expr[String])(body: c.Expr[Any]): c.Expr[A] = {
      val Expr(Literal(Constant(e: String))) = method
      val (listener, on, toOverride) = callbackPartsFor(Option(e))
      c.Expr(newListenerClass(listener, on, body.tree, toOverride, false, false))
    }

    def apply[A: c.WeakTypeTag](body: c.Expr[Any]): c.Expr[A] = {
      val (listener, on, toOverride) = callbackPartsFor(None)
      c.Expr(newListenerClass(listener, on, body.tree, toOverride, true, false))
    }

    def applyM[A: c.WeakTypeTag](method: c.Expr[String])(handler: c.Expr[Any]): c.Expr[A] = {
      val Expr(Literal(Constant(e: String))) = method
      val (listener, on, toOverride) = callbackPartsFor(Option(e))
      c.Expr(newListenerClass(listener, on, handler.tree, toOverride, true, false))
    }
  }

}
