package iota

import scala.reflect.internal.TreeGen
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
  private val OBJECT_FUNCTIONS = Set("clone", "toString", "hashCode", "equals", "finalize")
  import c.universe._

  def callbackPartsFor[V : c.WeakTypeTag](e: String, methodName: Option[String] = None) = {
    val tp = weakTypeOf[V]
    val setter = util.Try {
      tp.member(newTermName(s"setOn${e.capitalize}Listener")).asMethod
    }.recover { case _ =>
      tp.member(newTermName(s"addOn${e.capitalize}Listener")).asMethod
    }.recover { case _ =>
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
    // hack for partially implemented listeners, until we move to 2.11 only and can use isAbstract
    val toImplement = if (!listener.typeSymbol.asClass.isAbstractClass) Nil else listener.members.collect {
      case m: MethodSymbol if !m.isFinal && m.isJava && !m.isConstructor && !OBJECT_FUNCTIONS(m.name.encoded) && m.name.encoded != onMethod=>
        m
    }
    val on = util.Try {
      listener.member(newTermName(onMethod)).asMethod
    } getOrElse {
      c.abort(c.enclosingPosition, s"Unsupported event listener class in $setter (no $onMethod)")
    }

    (setter, listener, on, toImplement.toList)
  }
  def applyHook0[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[IO[Any]]): c.Expr[Kestrel[V]] = {
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on, toOverride) = callbackPartsFor(e)
    val anon = newListenerClass(listener, on, handler.tree, toOverride, false)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }
  def applyHookM0[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[IO[Any]]): c.Expr[Kestrel[V]] = {
    val Literal(Constant(outer: String)) = c.prefix.tree.collect { case Apply(n, xs) => xs }.head.last
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on, toOverride) = callbackPartsFor(outer, Option(e))

    val anon = newListenerClass(listener, on, handler.tree, toOverride)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }
  def applyHook[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[Any]): c.Expr[Kestrel[V]] = {
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on, toOverride) = callbackPartsFor(e)
    val anon = newListenerClass(listener, on, handler.tree, toOverride, true)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }
  def applyHookM[V: c.WeakTypeTag](event: c.Expr[String])(handler: c.Expr[Any]): c.Expr[Kestrel[V]] = {
    val Literal(Constant(outer: String)) = c.prefix.tree.collect { case Apply(n, xs) => xs }.head.last
    val Expr(Literal(Constant(e: String))) = event
    val (setter, listener, on, toOverride) = callbackPartsFor(outer, Option(e))

    val anon = newListenerClass(listener, on, handler.tree, toOverride, true)
    c.Expr[Kestrel[V]](newKestrel(weakTypeOf[V], setter.name.encoded, anon))
  }

  def newListenerClass(tpe: Type, sym: MethodSymbol, handler: Tree, overrides: List[MethodSymbol], handleArgs: Boolean = false) = {
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
            ) ++ newListenerOverrides(overrides)
          )

        )
      ),
      Apply(Select(New(Ident(newTypeName(listenerTypeName))), nme.CONSTRUCTOR), List())
    )
  }

  // semi-copied from TreeGen
  def mkZero(tp: Type): Tree = Literal(mkConstantZero(tp))
  def mkConstantZero(tp: Type): Constant =
    if (tp =:= typeOf[Unit])         Constant(())
    else if (tp =:= typeOf[Boolean]) Constant(false)
    else if (tp =:= typeOf[Float])   Constant(0.0f)
    else if (tp =:= typeOf[Float])   Constant(0.0d)
    else if (tp =:= typeOf[Byte])    Constant(0.toByte)
    else if (tp =:= typeOf[Short])   Constant(0.toShort)
    else if (tp =:= typeOf[Int])     Constant(0)
    else if (tp =:= typeOf[Long])    Constant(0L)
    else if (tp =:= typeOf[Char])    Constant(0.toChar)
    else                             Constant(null)

  def newListenerOverrides(overrides: List[MethodSymbol]): List[c.Tree] = {
    overrides map { sym =>
      val ret = mkZero(sym.returnType)
      val params = sym.paramss.head map { p =>
        ValDef(Modifiers(Flag.PARAM),
          newTermName(c.fresh()),
          TypeTree(p.typeSignature), EmptyTree)
      }
      DefDef(
        Modifiers(Flag.OVERRIDE),
        newTermName(sym.name.encoded),
        List(),
        List(params),
        TypeTree(),
        ret
      )
    }
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
