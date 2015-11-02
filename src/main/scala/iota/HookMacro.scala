package iota

import scala.reflect.macros.Context

/**
  * @author pfnguyen
  */
private[iota] object HookMacro {
  def applyHook0[V: c.WeakTypeTag](c: Context)(event: c.Expr[String])(handler: c.Expr[IO[Any]]): c.Expr[Kestrel[V]] = {
    val helper = new HookMacro[c.type](c)
    helper.applyHook0(event)(handler)
  }
  def applyHookM0[V: c.WeakTypeTag](c: Context)(event: c.Expr[String])(handler: c.Expr[IO[Any]]): c.Expr[Kestrel[V]] = {
    val helper = new HookMacro[c.type](c)
    helper.applyHookM0(event)(handler)
  }
  def applyHook[V: c.WeakTypeTag](c: Context)(event: c.Expr[String])(handler: c.Expr[Any]): c.Expr[Kestrel[V]] = {
    val helper = new HookMacro[c.type](c)
    helper.applyHook(event)(handler)
  }
  def applyHookM[V: c.WeakTypeTag](c: Context)(event: c.Expr[String])(handler: c.Expr[Any]): c.Expr[Kestrel[V]] = {
    val helper = new HookMacro[c.type](c)
    helper.applyHookM(event)(handler)
  }
}

private[iota] class HookMacro[C <: Context](val c: C) extends Internal210 {
  import c.universe._

  def callbackPartsFor[V : c.WeakTypeTag](e: String, methodName: Option[String] = None) = {
    val tp = weakTypeOf[V]
    val setter = util.Try {
      tp.member(newTermName(s"set${e.capitalize}Listener")).asMethod
    }.recover { case _ =>
      tp.member(newTermName(s"add${e.capitalize}Listener")).asMethod
    }.recover { case _ =>
      tp.member(newTermName(s"set${e.capitalize}")).asMethod
    }.recover { case _ =>
      tp.member(newTermName(e)).asMethod
    }.getOrElse(c.abort(c.enclosingPosition, s"no event listener for $e found in $tp"))
    val listener = setter.paramss.head.head.typeSignature

    val onMethod = methodName getOrElse e
    val on = util.Try {
      listener.member(newTermName(onMethod)).asMethod
    } getOrElse {
      c.abort(c.enclosingPosition, s"Unsupported event listener class in $setter (no $onMethod)")
    }

    (setter, listener, on)
  }
  def applyHook0[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[IO[Any]]): c.Expr[Kestrel[V]] = {
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on) = callbackPartsFor(e)
    val anon = newListenerClass(listener, on, handler.tree)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }
  def applyHookM0[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[IO[Any]]): c.Expr[Kestrel[V]] = {
    val Literal(Constant(outer: String)) = c.prefix.tree.collect { case Apply(n, xs) => xs }.head.last
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on) = callbackPartsFor(outer, Option(e))

    val anon = newListenerClass(listener, on, handler.tree)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }
  def applyHook[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[Any]): c.Expr[Kestrel[V]] = {
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on) = callbackPartsFor(e)
    val anon = newListenerClass(listener, on, handler.tree, true)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }
  def applyHookM[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[Any]): c.Expr[Kestrel[V]] = {
    val Literal(Constant(outer: String)) = c.prefix.tree.collect { case Apply(n, xs) => xs }.head.last
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on) = callbackPartsFor(outer, Option(e))

    val anon = newListenerClass(listener, on, handler.tree, true)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }

  def newListenerClass(tpe: Type, sym: MethodSymbol, handler: Tree, handleArgs: Boolean = false) = {
    val listenerTypeName = c.fresh()
    Block(
      List(
        ClassDef(
          Modifiers(Flag.FINAL), newTypeName(listenerTypeName), List(),
          Template(
            List(TypeTree(tpe)), emptyValDef,
            List(
              DefDef(
                Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
                Block(
                  List(
                    Apply(
                      Select(
                        Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR
                      ), List()
                    )
                  ), Literal(Constant(()))
                )
              ), newListenerMethod(sym, handler, handleArgs)
            )
          )
        )
      ),
      Apply(Select(New(Ident(newTypeName(listenerTypeName))), nme.CONSTRUCTOR), List())
    )
  }

  def newListenerMethod(sym: MethodSymbol, handler: Tree, handleArgs: Boolean = false) = {
    val params = sym.paramss.head map { p =>
      ValDef(Modifiers(Flag.PARAM),
        newTermName(c.fresh()),
        TypeTree(p.typeSignature), EmptyTree)
    }

    val h = spliceTree(c)(c.internal.enclosingOwner, handler)

    DefDef(
      Modifiers(Flag.OVERRIDE),
      newTermName(sym.name.encoded),
      List(),
      List(params),
      TypeTree(),
      Apply(Select(if (handleArgs) {
        Apply(Select(h, newTermName("apply")), params.map(p => Ident(p.name)))
      } else h, newTermName("perform")), Nil)
    )
  }

  def newKestrel(tpe: Type, method: String, handler: Tree) = {
    val vparam = c.fresh()
    Apply(
      Select(Ident(newTermName("iota")), newTermName("kestrel")),
      List(
        Function(
          List(ValDef(Modifiers(Flag.PARAM), newTermName(vparam), TypeTree(tpe), EmptyTree)),
          Apply(Select(Ident(newTermName(vparam)), newTermName(method)), List(handler))
        )
      )
    )
  }
}
