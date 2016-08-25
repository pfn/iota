package iota

import android.view.View
import android.view.ViewGroup
import android.content.{Context => AndroidContext}
import android.annotation.TargetApi
import scala.reflect.api.Universe

/**
  * @author pfnguyen
  */
private[iota] object ViewTreeMacro {
  import scala.reflect.macros.Context

  def nest[B <: ViewGroup : c.WeakTypeTag](c: Context)(views: c.Expr[View]*)(body: c.Expr[Any]): c.Expr[B] = {
    import c.universe._
    val t = weakTypeOf[B]

    val container = newTermName("container")
    val getContext = Apply(Select(Ident(container), newTermName("getContext")), Nil)
    val contextterm = newTermName(c.fresh("context"))

    val nvg = Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), Ident(contextterm):: Nil)
    val nvgterm = newTermName(c.fresh("viewgroup"))

    c.Expr(Block(
      List(
        ValDef(Modifiers(Flag.PARAM), contextterm, TypeTree(typeOf[AndroidContext]), getContext),
        ValDef(Modifiers(), nvgterm, TypeTree(t), nvg)
      ) ++ List(body.tree) ++
        views.map { v =>
          Apply(Select(Ident(nvgterm), newTermName("addView")), List(v.tree))
        },
      Ident(nvgterm)
    ))
  }
  def create[B <: View : c.WeakTypeTag](c: Context)(args: c.Expr[Any]*): c.Expr[B] = {
    import c.universe._
    val t = weakTypeOf[B]
    val getContext = Apply(Select(Ident(newTermName("container")), newTermName("getContext")), Nil)
    // TODO inspect ctors to see if it has necessary Context and AttributeSet params
    // ignore, skip, etc. if not present
    c.Expr(Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR),
      getContext :: Literal(Constant(null)) :: (args.toList.map(_.tree) ++ Nil)))
  }
  def create2[B <: View : c.WeakTypeTag](c: Context): c.Expr[B] = create[B](c)()

  def inflateBase[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext],
                                                inflater: c.Expr[Any],
                                                prefix: List[String],
                                                factoryTerm: Option[c.TermName],
                                                factory: Option[c.Expr[PartialFunction[String,View]]]): c.Expr[A] = {
    import c.universe._
    val vgt = c.weakTypeOf[ViewGroup]
    val vwt = c.weakTypeOf[View]
    val vtt = c.weakTypeOf[ViewTree[_]]
    val act = c.weakTypeOf[AndroidContext]
    val wtt = implicitly[WeakTypeTag[A]]

    // yuck, refactor this
    val factoryVal = factoryTerm.map(t => Some(t) -> Option.empty[Tree]).getOrElse(
      factory.fold((Option.empty[TermName],Option.empty[Tree])) { f =>
        val fterm = newTermName(c.fresh("factory"))
        val ftree = reify {
          f.splice.lift
        }.tree
        Some(fterm) ->
          Some(ValDef(Modifiers(Flag.PARAM), fterm, TypeTree(typeOf[String => Option[View]]), ftree))
      })

    val inputs = inflater.tree match {
      case Block(_, Function(in, _)) => in.map(i => (i.name,i.tpt, None))
      case TypeTree() | Select(_, _) | Ident(_) =>
        val applySym = inflater.tree.symbol.typeSignature.member(newTermName("apply")).asMethod
        // handle default parameters
        applySym.paramss.head.zipWithIndex.map { case (p,i) =>
          (newTermName(p.name.encoded), TypeTree(p.typeSignature),
            if (p.asTerm.isParamWithDefault) {
              import scala.reflect.internal.{Definitions, SymbolTable, StdNames}
              val u = c.universe.asInstanceOf[Definitions with SymbolTable with StdNames]
              val getter = u.nme.defaultGetterName(u.newTermName("apply"), i + 1)
              Option(Select(Ident(inflater.tree.symbol), newTermName(getter.encoded)))
            }
            else None
          )
        }
    }

    def addView(vg: Tree, view: Tree): Tree = {
      val v = c.Expr[View](view)
      val g = c.Expr[ViewGroup](vg)
      val a = c.Expr[Unit](Apply(Select(vg, newTermName("addView")), List(view)))
      // support the possibility of adding an anonymous level in the view hierarchy
      reify {
        if (v.splice.getParent != null && v.splice.getParent != g.splice) {
          if (v.splice.getParent.getParent == null)
            g.splice.addView(v.splice.getParent.asInstanceOf[ViewGroup])
        } else
          a.splice
      }.tree
    }

    val (args, sts, addviews, vgs) = inputs.foldLeft(
      (List.empty[Tree], List.empty[Tree], List.empty[Tree => Tree],Option.empty[Tree])) {
      case ((a,add,addview,vg),(inn,int,deft)) =>
        val t = int.tpe

        def withFactory(view: Tree): Tree = {
          val viewExpr = c.Expr[View](view)
          factoryVal._1.fold(view) { fact =>
            val key = c.Expr[String](Literal(Constant((inn.encoded :: prefix).reverse.mkString("."))))
            val f = c.Expr[String => Option[View]](Ident(fact))
            val withfactory = reify {
              f.splice.apply(key.splice).getOrElse(viewExpr.splice)
            }
            val tpe = c.Expr[String](Literal(Constant(t.toString)))
            val facOut = TypeApply(Select(withfactory.tree, newTermName("asInstanceOf")), List(TypeTree(t)))
            val facterm = newTermName(c.fresh())
            val facblock = c.Expr[View](Block(
              List(
                ValDef(Modifiers(Flag.PARAM), facterm, TypeTree(), facOut),
                If(
                  Apply(Select(Ident(facterm), newTermName("$eq$eq")), List(Literal(Constant(null)))),
                  reify(throw new NullPointerException(s"'${key.splice}' resulted in a null value")).tree,
                  Literal(Constant(())))
              ),
              Ident(facterm)
            ))
            reify {
              try {
                facblock.splice
              } catch {
                case e: ClassCastException =>
                  val ex = new ClassCastException(s"Failed to cast '${key.splice}' to ${tpe.splice}")
                  ex.initCause(e)
                  throw ex
              }
            }.tree
          }
        }

        if (deft.nonEmpty) {
          val newterm = newTermName(c.fresh("defview"))
          val sel = Ident(newterm)

          val newv = ValDef(Modifiers(Flag.PARAM), newterm, TypeTree(t), withFactory(deft.get))

          val vgadd: Tree => Tree = addView(_, sel)

          (sel :: a, newv :: add, vgadd :: addview, vg)
        } else if (inn.encoded == "container" && t <:< vgt) {
          if (vg.isDefined)
            c.abort(c.enclosingPosition, s"'container' has already been defined in $inn?!?!")

          val newterm = newTermName(c.fresh("viewgroup"))
          val sel = Ident(newterm)

          val nvg = Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil)
          val newvgVal = ValDef(Modifiers(Flag.PARAM), newterm, TypeTree(t), withFactory(nvg))

          (sel :: a, newvgVal :: add, addview, Some(sel))
        } else if (t <:< vwt) {
          val newterm = newTermName(c.fresh("view"))
          val sel = Ident(newterm)

          val newView =
            Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil)
          val newv = ValDef(Modifiers(Flag.PARAM), newterm, TypeTree(t), withFactory(newView))

          val vgadd: Tree => Tree = addView(_, sel)
          (sel :: a, newv :: add, vgadd :: addview, vg)
        } else if (t =:= act) { // inject a context parameter into arg list if requested
          (ctx.tree :: a, add, addview, vg)
        } else if (t <:< vtt) {
          // WeakTypeTag hack so that nested valdef has correct type
          val tree = inflateBase(c)(ctx, c.Expr(nestedApply(c)(t)), inn.encoded :: prefix, factoryVal._1, factory)(new WeakTypeTag[A] {
            override val mirror = rootMirror
            override def in[U <: Universe with Singleton](otherMirror: reflect.api.Mirror[U]) = sys.error("nice try")
            override def tpe = t.typeSymbol.asType.toType
          })
          val newterm = newTermName(c.fresh("viewtree"))
          val sel = Ident(newterm)
          val newvt = ValDef(Modifiers(Flag.PARAM), newterm, TypeTree(t), tree.tree)
          val vgadd: Tree => Tree = addView(_, Select(sel, newTermName("container")))

          (sel :: a, newvt :: add, vgadd :: addview, vg)
        } else if (t <:< typeOf[Option[_]]) { // fill in Option holes with None
          (Ident(typeOf[None.type].typeSymbol.companionSymbol) :: a, add, addview, vg)
        } else {
          val container = Option(inflater.tree.symbol).fold("<anon>")(_.fullName)
          c.abort(inflater.tree.pos,
            s"parameter '$inn: $t' in $container is not supported, " +
              "only android.view.View and iota.ViewTree subclasses are allowed")
        }
    }
    if (vgs.isEmpty) {
      val container = Option(inflater.tree.symbol).fold("<anon>")(_.fullName)
      c.abort(inflater.tree.pos, s"missing 'container' field in $container")
    }
    val vtterm = newTermName(c.fresh("vt"))
    val newvt = ValDef(Modifiers(Flag.PARAM),
      vtterm,
      TypeTree(wtt.tpe),
      Apply(inflater.tree, args.reverse)
    )
    c.Expr(
      Block(
        factoryVal._2.toList ++ sts.reverse ++ List(newvt) ++ vgs.toList.flatMap(v => addviews.reverse.map(_.apply(v))),
        Ident(vtterm)
      )
    )
  }

  def nestedApply(c: Context)(t: c.Type): c.Tree = {
    import c.universe._
    val ownerTree = if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.owner)
    else if (t.typeSymbol.owner.isClass) This(t.typeSymbol.owner)
    else EmptyTree
    if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.companionSymbol)
    else Select(ownerTree, t.typeSymbol.companionSymbol)
  }

  def inflaterSymbols(c: Context)(inflater: c.Tree, prefix: List[String]): Either[(c.Position,String),Map[String,c.Type]] = {
    import c.universe._
    inflater match {
      case TypeTree() | Select(_, _) | Ident(_) =>
        val applySym = inflater.symbol.typeSignature.member(newTermName("apply")).asMethod

        val rs = applySym.paramss.head.zipWithIndex.map { case (p,i) =>
          if (p.typeSignature <:< typeOf[View]) {
            Right(Map((p.name.encoded :: prefix).reverse.mkString(".") -> p.typeSignature))
          } else if (p.typeSignature <:< typeOf[ViewTree[_]]) {
            inflaterSymbols(c)(nestedApply(c)(p.typeSignature), p.name.encoded :: prefix)
          } else {
            Right(Map.empty)
          }
        }
        rs.foldLeft(Right(Map.empty) :Either[(c.Position,String),Map[String,c.Type]]) {
          case (a,r) =>
            for {
              ac <- a.right
              m <- r.right
            } yield ac ++ m
        }
      case _ => Left((inflater.pos, "ViewTree factory cannot be inspected for typesafety"))
    }
  }

  def checkFactory[A: c.WeakTypeTag](c: Context)(inflater: c.Expr[Any])(factory: c.Expr[PartialFunction[String,View]]): c.Expr[PartialFunction[String,View]] = {
    import c.universe._
    val callsite = enclosingTrees(c).head
    val nowarn = callsite.symbol.annotations.exists(_.tpe =:= typeOf[ViewTree.UnsafeOperation])
    type Pattern = (Option[String], Tree, Tree, Option[(Position,String)])
    val found = factory.tree.collect {
      case DefDef(_, name, _, vps, tpt, rhs) if name.encoded == "applyOrElse" =>
        rhs
    }.headOption

    val syms = inflaterSymbols(c)(inflater.tree, Nil)
    import scala.reflect.internal.{Definitions, SymbolTable, StdNames}
    val u = c.universe.asInstanceOf[Definitions with SymbolTable with StdNames]

    def handlePattern(pat: Tree, body: Tree): List[Pattern] = pat match {
      case Bind(name, _) =>
        if (name.encoded == u.nme.DEFAULT_CASE.encoded) Nil
        else List((None, pat, body, Some((pat.pos, "wildcard ViewTree factory patterns cannot be typechecked"))))
      case Literal(Constant(s: String)) =>
        List((Some(s), pat, body, None))
      case Alternative(pats) =>
        pats.flatMap(handlePattern(_, body))
      case y =>
        List((None, pat, body, Some((pat.pos, "this ViewTree factory case pattern cannot be typechecked"))))
    }

    val cases: List[Pattern] = found.toList.flatMap(_.collect {
      case CaseDef(pat, guard, body) =>
        handlePattern(pat, body)
      case _ => Nil
    }.flatten)

    if (!nowarn) {
      syms.left.foreach { case (pos, w) =>
        c.warning(pos, w)
      }
      if (found.isEmpty)
        c.warning(factory.tree.pos, "Embedded ViewTree factory functions cannot be checked for type-safety")
      cases.foreach {
        case (_, _, _, Some((pos, s))) => c.warning(pos, s)
        case _ =>
      }
    }
    val nothing = typeOf[Nothing]
    val nul = typeOf[Null]
    def isEmpty(t: Type) = t <:< nothing || t <:< nul
    val symbols = syms.fold(_ => Map.empty[String, c.Type], identity)
    val errors = cases.foldLeft(List.empty[(c.Position,String)]) {
      case (errs, (n, pat, body, _)) => n.toList.flatMap { nm =>
        symbols.get(nm).fold(List(pat.pos -> s"not found in ${inflater.tree.symbol.companionSymbol}: pattern '$nm'")) { tpe =>
          if (!(body.tpe <:< tpe) || isEmpty(body.tpe)) {
            List(body.pos -> (s"ViewTree type mismatch;\n" +
              s" found    : ${body.tpe}\n" +
              s" required : $tpe"))
          }
          else Nil
        }
      } ++ errs
    }
    if (errors.nonEmpty) {
      errors.foreach { case (pos, e) => c.error(pos, e) }
      c.abort(c.enclosingPosition, "ViewTree factory type inspection failed")
    }

    factory
  }
  def inflateWithFactory[A: c.WeakTypeTag](c: Context)
                                          (ctx: c.Expr[AndroidContext], inflater: c.Expr[Any])
                                          (factory: c.Expr[PartialFunction[String,View]]): c.Expr[A] = {
    checkFactory[A](c)(inflater)(factory)
    inflateBase(c)(ctx, inflater, Nil, None, Some(factory))
  }

  def inflateAny[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[Any]): c.Expr[A] = {
    inflateBase(c)(ctx, inflater, Nil, None, None)
  }

  val matchWrapValues = Map(
    "matchWidth"          -> ViewGroup.LayoutParams.MATCH_PARENT,
    "matchHeight"         -> ViewGroup.LayoutParams.MATCH_PARENT,
    "wrapWidth"           -> ViewGroup.LayoutParams.WRAP_CONTENT,
    "wrapHeight"          -> ViewGroup.LayoutParams.WRAP_CONTENT
  )
  val layoutParamFieldOps = Map(
    "rowSpec"             -> "rowSpec",
    "colSpec"             -> "columnSpec",
    "matchWidth"          -> "width",
    "matchHeight"         -> "height",
    "wrapWidth"           -> "width",
    "wrapHeight"          -> "height",
    "weight"              -> "weight",
    "gravity"             -> "gravity",
    "marginRight"         -> "rightMargin",
    "marginLeft"          -> "leftMargin",
    "marginTop"           -> "topMargin",
    "marginBottom"        -> "bottomMargin"
  )

  import android.widget.RelativeLayout
  val relativeLayoutParamViewOps = Map(
    "above"             -> RelativeLayout.ABOVE,
    "alignBaseLine"     -> RelativeLayout.ALIGN_BASELINE,
    "alignBottom"       -> RelativeLayout.ALIGN_BOTTOM,
    "alignEnd"          -> RelativeLayout.ALIGN_END,
    "alignLeft"         -> RelativeLayout.ALIGN_LEFT,
    "alignParentBottom" -> RelativeLayout.ALIGN_PARENT_BOTTOM,
    "alignParentEnd"    -> RelativeLayout.ALIGN_PARENT_END,
    "alignParentLeft"   -> RelativeLayout.ALIGN_PARENT_LEFT,
    "alignParentRight"  -> RelativeLayout.ALIGN_PARENT_RIGHT,
    "alignParentStart"  -> RelativeLayout.ALIGN_PARENT_START,
    "alignParentTop"    -> RelativeLayout.ALIGN_PARENT_TOP,
    "alignRight"        -> RelativeLayout.ALIGN_RIGHT,
    "alignStart"        -> RelativeLayout.ALIGN_START,
    "alignTop"          -> RelativeLayout.ALIGN_TOP,
    "below"             -> RelativeLayout.BELOW,
    "centerHorizontal"  -> RelativeLayout.CENTER_HORIZONTAL,
    "centerInParent"    -> RelativeLayout.CENTER_IN_PARENT,
    "centerVertical"    -> RelativeLayout.CENTER_VERTICAL,
    "endOf"             -> RelativeLayout.END_OF,
    "leftOf"            -> RelativeLayout.LEFT_OF,
    "rightOf"           -> RelativeLayout.RIGHT_OF,
    "startOf"           -> RelativeLayout.START_OF
  )

  def enclosingTrees(c: Context): List[c.Tree] = {
    c.asInstanceOf[reflect.macros.runtime.Context].callsiteTyper.context.enclosingContextChain.map(_.tree.asInstanceOf[c.Tree])
  }

  def findNestLayoutOf(c: Context) = {
    // ASSUMPTION nest will only ever occur inside a ViewTree case class
    // LIMITATION if nest[] occurs outside of case class A extends ViewTree this behaves badly
    import c.universe._
    val trees = enclosingTrees(c)
    val target = c.macroApplication
    val pf: PartialFunction[Tree, Name] = {
      case Apply(Apply(TypeApply(Ident(name), List(Ident(tpe))), List(views)), body)
        if name.encoded == "nest" && body.exists(b => b.pos == target.pos || b.children.exists(_.pos == target.pos)) => tpe
    }
    (target.collect(pf) ++ trees.foldLeft(List.empty[c.Name]) { (a, t) =>
      if (a.isEmpty)
        t.collect(pf)
      else a
    }).headOption.map { tn =>
      // reify TypeName to Type, hack because typeCheck expects a term
      c.typeCheck(Typed(Throw(Literal(Constant(null))), Ident(tn))).tpe
    }
  }

  def typeParamOf(c: Context)(p: c.Type) = p match {
    case c.universe.TypeRef(_, _, arg :: Nil) =>  arg
  }

  def checkLayoutConstraint(c: Context)(op: String, lpt: c.Type): Unit = {
    import c.universe._
    val lt = layoutType(c, op)
    val lpc = typeOf[ViewTree.LayoutParamConstraint[_]]
    val lc = typeOf[ViewTree.LayoutConstraint[_]]
    val lc2 = typeOf[ViewTree.LayoutConstraint2[_]]
    val owner = c.macroApplication.symbol.owner
    val lpcType = owner.typeSignature.baseType(lpc.typeSymbol)
    if (lpcType != NoType && !(lpt <:< typeParamOf(c)(lpcType))) {
      c.abort(c.macroApplication.pos, s"$op cannot be used in $lpt only in ${typeParamOf(c)(lpcType)}")
    }
    val lcType = owner.typeSignature.baseType(lc.typeSymbol)
    val lc2Type = owner.typeSignature.baseType(lc2.typeSymbol)
    val lcs = List(lcType, lc2Type).filter(_ != NoType).map(x => typeParamOf(c)(x))
    if (lcs.nonEmpty && !lcs.exists(lt <:< _)) {
      c.abort(c.macroApplication.pos, s"'.$op' cannot be used in $lt only in ${lcs.mkString(" or ")}")
    }
  }

  def withLayoutParams[B: c.WeakTypeTag](c: Context)(op: String, lpt: c.Type, stas: List[c.Tree => c.Tree], sts: List[c.Tree]): c.Expr[B] = {
    import c.universe._
    val defparams = c.Expr[ViewGroup.LayoutParams](defaultLp(c, op))
    val view = c.prefix.tree.children.last
    val viewterm = newTermName(c.fresh("view"))
    val viewdef = ValDef(Modifiers(Flag.PARAM), viewterm, TypeTree(c.weakTypeOf[B]), view)
    val vsplice = c.Expr[View](Ident(viewterm))
    val checklptype = c.Expr[Boolean](
      TypeApply(Select(
        Apply(Select(vsplice.tree, newTermName("getLayoutParams")), Nil),
        newTermName("isInstanceOf")), List(TypeTree(lpt))))
    val ensureLp = reify {
      if (vsplice.splice.getLayoutParams == null || !checklptype.splice)
        vsplice.splice.setLayoutParams(defparams.splice)
      vsplice.splice.getLayoutParams
    }.tree
    val vterm = newTermName(c.fresh("lp"))
    val vdef = ValDef(Modifiers(Flag.PARAM), vterm, TypeTree(lpt),
      TypeApply(Select(ensureLp, newTermName("asInstanceOf")), List(TypeTree(lpt)))
    )
    c.Expr[B](Block(
      viewdef :: vdef :: (sts ++ stas.map(_.apply(Ident(vterm)))),
      Ident(viewterm)
    ))
  }

  def layoutParamField[A : c.WeakTypeTag](c: Context)(value: c.Expr[Any]): c.Expr[A] = {
    import c.universe._
    val (op, lpt) = commonLayoutConstraints(c)
    val intParam: Tree => Tree  = t =>
      Assign(Select(t, newTermName(layoutParamFieldOps(op))), value.tree)
    withLayoutParams(c)(op, lpt, intParam :: Nil, Nil)
  }

  def layoutParamField2[A : c.WeakTypeTag](c: Context)(): c.Expr[A] = {
    import c.universe._
    val (op, _) = commonLayoutConstraints(c)
    layoutParamField(c)(c.Expr(Literal(Constant(matchWrapValues(op)))))
  }

  def withTargetApi[B: c.WeakTypeTag](c: Context)(tree: c.Expr[B], view: c.Expr[B]): c.Expr[B] = {
    import c.universe._
    val target = c.macroApplication.symbol.annotations.find(_.tpe <:< typeOf[TargetApi]).flatMap {
      _.javaArgs.values.collectFirst {
        case LiteralArgument(Constant(i: Int)) => i
      }
    }
    target.fold(tree) { t =>
      val vers = c.Expr[Int](Literal(Constant(t)))
      reify {
        if (android.os.Build.VERSION.SDK_INT >= vers.splice)
          tree.splice
        else
          view.splice
      }
    }
  }

  def commonLayoutConstraints(c: Context) = {
    val op = c.macroApplication.symbol.name.encoded
    val lpt = layoutParamType(c, op)
    checkLayoutConstraint(c)(op, lpt)
    (op, lpt)
  }

  def relativeLayoutParamView[A : c.WeakTypeTag](c: Context)(view: c.Expr[View]): c.Expr[A] = {
    import c.universe._
    val (op, lpt) = commonLayoutConstraints(c)
    val ensureId = reify {
      if (view.splice.getId == android.view.View.NO_ID) {
        view.splice.setId(System.identityHashCode(view.splice))
        view.splice.getId
      } else
        view.splice.getId
    }.tree
    val iterm = newTermName(c.fresh("id"))
    val idef = ValDef(Modifiers(Flag.PARAM), iterm, TypeTree(typeOf[Int]), ensureId)
    val addRule: Tree => Tree = t =>
      Apply(Select(t, newTermName("addRule")), List(Literal(Constant(relativeLayoutParamViewOps(op))), Ident(iterm)))
    val x = withLayoutParams(c)(op, lpt, addRule :: Nil, idef :: Nil)
    withTargetApi(c)(x, c.Expr(c.prefix.tree.children.last))
  }

  def relativeLayoutUnary[A : c.WeakTypeTag](c: Context)(): c.Expr[A] = {
    import c.universe._
    val (op, lpt) = commonLayoutConstraints(c)
    val addRule: Tree => Tree = t =>
      Apply(Select(t, newTermName("addRule")), List(Literal(Constant(relativeLayoutParamViewOps(op))), Literal(Constant(RelativeLayout.TRUE))))
    val x = withLayoutParams(c)(op, lpt, addRule :: Nil, Nil)
    withTargetApi(c)(x, c.Expr(c.prefix.tree.children.last))
  }

  def relativeLayoutAlignParent[A : c.WeakTypeTag](c: Context)(): c.Expr[A] = {
    import c.universe._
    val (op, lpt) = commonLayoutConstraints(c)
    val addRule: Tree => Tree = t =>
      Assign(Select(t, newTermName("alignWithParent")), Literal(Constant(true)))
    withLayoutParams(c)(op, lpt, addRule :: Nil, Nil)
  }

  def layoutType(c: Context, op: String): c.Type = {
    import c.universe._
    val vgt = c.weakTypeOf[ViewGroup]
    val vtt = typeOf[ViewTree[_]]
    val vts = c.enclosingClass.symbol.typeSignature.baseType(vtt.typeSymbol)

    if (vts == NoType)
      c.abort(c.enclosingPosition, s"'$op' can only be used in a subclass of iota.ViewTree")
    findNestLayoutOf(c).getOrElse(typeParamOf(c)(vts)).get
  }

  def layoutParamType(c: Context, op: String): c.Type =
    IOViewGroupMacro.lpTypeOf(c)(layoutType(c, op)).toType

  def defaultLp(c: Context, op: String): c.Tree = {
    import ViewGroup.LayoutParams.WRAP_CONTENT
    import c.universe._
    makeLp(c, op)(c.Expr(Literal(Constant(WRAP_CONTENT))), c.Expr(Literal(Constant(WRAP_CONTENT))))
  }

  def makeLp(c: Context, op: String)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    val ctor = layoutParamType(c, op).member(nme.CONSTRUCTOR).asTerm
    val ctors = if (ctor.isOverloaded) {
      ctor.alternatives.map(_.asMethod)
    } else
      List(ctor.asMethod)

    val argtypes = args.map(e => c.typeCheck(e.tree).tpe).toList
    val matchingCtor: MethodSymbol => Boolean = a => {
      val ptypes = a.paramss.head.map(_.typeSignature)
      val zipped = argtypes.zip(ptypes)
      zipped.size == argtypes.size && zipped.forall { case (x, y) => x weak_<:< y }
    }
    val hasCtor = ctors.exists(matchingCtor)

    val hasFallbackCtor = !hasCtor && {
      typeOf[ViewGroup.LayoutParams].member(nme.CONSTRUCTOR).asTerm.alternatives.map(_.asMethod) exists matchingCtor
    }

    // allow creating defaults for GridLayout.LayoutParams
    // if no fallback, attempt to use real constructors for best error messages
    if (hasCtor || !hasFallbackCtor) {
      Apply(Select(
        New(TypeTree(layoutParamType(c, op))), nme.CONSTRUCTOR), args.map(_.tree).toList)
    } else {
      val base = Apply(Select(
        New(TypeTree(typeOf[ViewGroup.LayoutParams])), nme.CONSTRUCTOR), args.map(_.tree).toList)
      Apply(Select(
        New(TypeTree(layoutParamType(c, op))), nme.CONSTRUCTOR), List(base))
    }
  }

  def lp[A : c.WeakTypeTag](c: Context)(args: c.Expr[Any]*): c.Expr[A] = {
    import c.universe._
    val view = c.prefix.tree.children.last
    val lp = makeLp(c, "lp")(args:_*)
    val vterm = newTermName(c.fresh("view"))
    val vdef = ValDef(Modifiers(Flag.PARAM), vterm, TypeTree(c.weakTypeOf[A]), view)
    c.Expr(Block(List(vdef, Apply(Select(view, newTermName("setLayoutParams")), List(lp))), Ident(vterm)))
  }
}
