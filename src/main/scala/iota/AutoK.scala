package iota

import scala.reflect.macros.Context

/**
  * @author pfnguyen
  */
private[iota] trait AutoK {
  import language.dynamics

  /**
    * K-combinator generator, use with any IO container to generate
    * a kestrel automatically, e.g. `w[TextView] >>= k.hint("Hint")` turns into:
    * `w[TextView] >>= kestrel(_.setHint("Hint"))`
    *
    * Another example: `IO(new StringBuilder) >>= k.append("Foo")` turns into:
    * `IO(new StringBuilder) >>= kestrel(_.append("Foo"))`
    *
    * Rules for resolution of the containing object's method are:
    *  `setOnNAMEListener`,
    *  `addOnNAMEListener`,
    *  `setNAMEListener`,
    *  `addNAMEListener`,
    *  `setNAME`,
    *  `NAME`
    */
  object k extends Dynamic {
    def applyDynamic[V,A](method: String)(args: A*): Kestrel[V] = macro AutoKMacro.applyK[V,A]
  }
}
private[iota] object AutoKMacro {
  def applyK[V: c.WeakTypeTag, A](c: Context)(method: c.Expr[String])(args: c.Expr[Any]*): c.Expr[Kestrel[V]] = {
    val helper = new AutoKMacro[c.type](c)
    helper.applyK(method)(args)
  }

  private[iota] class AutoKMacro[C <: Context](val c: C) extends Internal210 {

    import c.universe._

    def setterFor[V: c.WeakTypeTag](e: String, methodName: Option[String] = None) = {
      val tp = weakTypeOf[V]
      val checkNoSymbol: Symbol => util.Try[Symbol] =
        s => if (s == NoSymbol) util.Failure(new Exception) else util.Success(s)
      val r = util.Try {
        tp.member(newTermName(s"setOn${e.capitalize}Listener"))
      }.flatMap(checkNoSymbol).recover { case x =>
        tp.member(newTermName(s"addOn${e.capitalize}Listener"))
      }.flatMap(checkNoSymbol).recover { case x =>
        tp.member(newTermName(s"set${e.capitalize}Listener"))
      }.flatMap(checkNoSymbol).recover { case x =>
        tp.member(newTermName(s"add${e.capitalize}Listener"))
      }.flatMap(checkNoSymbol).recover { case x =>
        tp.member(newTermName(s"set${e.capitalize}"))
      }.flatMap(checkNoSymbol).recover { case x =>
        tp.member(newTermName(e))
      }.flatMap(checkNoSymbol).getOrElse(
        c.abort(c.enclosingPosition, s"no method for $e found in $tp"))
      r
    }

    def applyK[V: c.WeakTypeTag, A](method: c.Expr[String])(args: Seq[c.Expr[Any]]): c.Expr[Kestrel[V]] = {
      val Expr(Literal(Constant(e: String))) = method
      c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setterFor(e).name.encoded, args))
    }

    def newKestrel(tpe: Type, method: String, args: Seq[c.Expr[Any]]) = {
      val vparam = c.fresh()
      val f = c.Expr(Function(
        List(ValDef(Modifiers(Flag.PARAM), newTermName(vparam), TypeTree(tpe), EmptyTree)),
        Apply(Select(Ident(newTermName(vparam)), newTermName(method)), args.map(_.tree).toList)
      ))
      reify(iota.std.Combinators.kestrel(f.splice)).tree
    }
  }
}
