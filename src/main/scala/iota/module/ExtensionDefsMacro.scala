package iota.module

import iota.{HookMacro, SingleMacro}

/**
  * @author pfnguyen
  */
private[iota] object ExtensionDefsMacro {
  import scala.reflect.macros.Context

  // TODO handle registration calls that take more than just a listener class,
  // e.g. AlertDialog's setButton(int, CharSequence, DialogInterface.OnClickListener)
  // could be rewritten as onClick(int, CharSequence)(onClickFunction)
  def materializeTypeclassInstance[C[_],A : c.WeakTypeTag](c: Context)(implicit ctag: c.WeakTypeTag[C[A]]): c.Expr[C[A]] = {
    import c.universe._
    val config = c.macroApplication.symbol.annotations.find(_.tpe <:< typeOf[AndroidTypeclass])
    val (test, toImp) = config.fold(c.abort(c.enclosingPosition, s"AndroidTypeclass was not defined on ${c.macroApplication}")) { cfg =>
      cfg.scalaArgs.zipWithIndex.foldLeft((List.empty[String],"")) { case ((checks, imps), (a,i)) =>
        if (i == 0) {
          (a.collect {
            case Apply(_, args) =>
              args.collect { case Literal(Constant(n: String)) => n }
          }.flatten, imps)
        } else if (i == 1) {
          val Literal(Constant(toImplement: String)) = a
          (checks, toImplement)
        } else c.abort(cfg.scalaArgs.head.pos, "Wrong number of arguments")
      }
    }
    val viewType = weakTypeOf[A]
    val targetTpe = ctag.tpe
    val setsym = test.map(t => viewType.member(newTermName(t))).find(_ != NoSymbol).map(_.asMethod)
    if (setsym.isEmpty) {
      c.abort(c.enclosingPosition, s"No registrant found on $viewType for '${test.mkString(" or ")}'")
    }

    val setter = setsym.get
    if (setter.paramss.isEmpty || setter.paramss.head.isEmpty)
      c.abort(c.enclosingPosition, s"'${setter.name}' does not take params")
    val listenerType = setter.paramss.head.head.typeSignature

    val anon = newTypeName(c.fresh("materialized"))
    val anonterm = newTermName(c.fresh())

    val toImplement = targetTpe.members.collect {
      case m: MethodSymbol if !m.isFinal && !m.isConstructor && !HookMacro.OBJECT_FUNCTIONS(m.name.encoded) => m
    }
    val h = new SingleMacro.SingleMacro[c.type](c)
    val newlisteners = toImplement.map { ti =>
      val tds = ti.typeParams.map { t =>
        val TypeBounds(lo, hi) = t.typeSignature
        TypeDef(Modifiers(Flag.PARAM), t.name.toTypeName, Nil, TypeBoundsTree(Ident(lo.typeSymbol.name), Ident(hi.typeSymbol.name)))
      }
      val tipss = ti.paramss.zipWithIndex.map { case (ps,i) =>
        ps.map { p =>
          val t = p.asTerm
          val args = t.typeSignature match {
            case TypeRef(_, k, targ :: Nil) if k == definitions.ByNameParamClass => Some(targ)
            case _ => None
          }

          val viewtarget = if (i == 0 || t.typeSignature.typeSymbol.name.encoded == "A") {
            TypeTree(viewType)
          } else {
            t.typeSignature match {
              case TypeRef(pre, sym, ts) =>
                AppliedTypeTree(Ident(sym), ts.map(t => Ident(t.typeSymbol.name)))
                AppliedTypeTree(Ident(sym), ts.map { t =>
                  if (t.typeSymbol.isParameter)
                    Ident(t.typeSymbol.name)
                  else
                    TypeTree(t)
                })
              case y => TypeTree(y)
            }
          }
          ValDef(
            if (t.isByNameParam) Modifiers(Flag.PARAM | Flag.BYNAMEPARAM) else Modifiers(Flag.PARAM),
            t.name.toTermName,
            if (t.isByNameParam)
              AppliedTypeTree(Ident(definitions.ByNameParamClass.name), Ident(args.map(_.typeSymbol.name).get) :: Nil)
            else viewtarget,
            EmptyTree)
        }
      }
      val m = listenerType.member(newTermName(toImp)).asMethod
      val target = ti.paramss.head.head
      val p = ti.paramss.drop(1).head.head.asTerm
      val listener = h.newListenerClass(listenerType,
        h.needImplementation(listenerType).filterNot(_.name.encoded == toImp),
        newListenerMethod(c)(m,
          Ident(p.name),
          !p.isByNameParam))
      val body = Apply(Select(Ident(target.name), setter.name.toTermName), listener :: Nil)
      DefDef(
        Modifiers(NoFlags, tpnme.EMPTY, List(
          Apply(Select(New(TypeTree(typeOf[scala.inline])), nme.CONSTRUCTOR), List())
        )),
        ti.name,
        tds,
        tipss,
        TypeTree(typeOf[Unit]),
        body)
    }
    val x = c.Expr[C[A]](Block(List(
      ClassDef(Modifiers(Flag.FINAL), anon, Nil,
        Template(
          List(AppliedTypeTree(Ident(targetTpe.typeSymbol), TypeTree(viewType) :: Nil)),
          emptyValDef,
          DefDef(
            Modifiers(),
            nme.CONSTRUCTOR,
            Nil,
            List(List()),
            TypeTree(),
            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))
          ) :: newlisteners.toList
        )
      ),
      ValDef(Modifiers(Flag.PARAM), anonterm, AppliedTypeTree(Ident(targetTpe.typeSymbol), TypeTree(viewType) :: Nil),Apply(Select(New(Ident(anon)), nme.CONSTRUCTOR), Nil))
    ), Ident(anonterm)))
    x
  }
  private[this] def newListenerMethod(c: Context)(sym: c.universe.MethodSymbol, handler: c.Tree, handleArgs: Boolean) = {
    import c.universe._
    val params = sym.paramss.head map { p =>
      ValDef(Modifiers(Flag.PARAM),
        newTermName(c.fresh()),
        TypeTree(p.typeSignature), EmptyTree)
    }

    val handle = if (handleArgs) {
      Apply(Select(handler, newTermName("apply")), params.map(p => Ident(p.name)))
    } else handler

    DefDef(
      Modifiers(Flag.OVERRIDE),
      sym.name.toTermName,
      List(),
      List(params),
      TypeTree(),
      handle
    )
  }
}
