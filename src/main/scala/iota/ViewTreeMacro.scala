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

  private[iota] class ViewTreeMacro[C <: Context](val c: C) {
    import c.universe._
    private[iota] def inflateAny[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[Any]): c.Expr[A] = {
      val vgt = c.weakTypeOf[ViewGroup]
      val vwt = c.weakTypeOf[View]
      val vtt = c.weakTypeOf[ViewTree[_]]
      val act = c.weakTypeOf[AndroidContext]
      val wtt = implicitly[WeakTypeTag[A]]

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
          if (v.splice.getParent != null && v.splice.getParent != g.splice)
            g.splice.addView(v.splice.getParent.asInstanceOf[ViewGroup])
          else
            a.splice
        }.tree
      }
      val (args, sts, addviews, vgs) = inputs.foldLeft(
        (List.empty[Tree], List.empty[Tree], List.empty[Tree => Tree],Option.empty[Tree])) {
        case ((a,add,addview,vg),(inn,int,deft)) =>
          val t = int.tpe
          if (deft.nonEmpty) {
            val newterm = newTermName(c.fresh("defview"))
            val sel = Ident(newterm)
            val newv = ValDef(Modifiers(Flag.PARAM), newterm, TypeTree(t), deft.get)
            val vgadd: Tree => Tree = addView(_, sel)
            (sel :: a, newv :: add, vgadd :: addview, vg)
          } else if (inn.encoded == "container" && t <:< vgt) {
            val nvg = Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil)
            val newterm = newTermName(c.fresh("viewgroup"))
            val newvg = ValDef(Modifiers(Flag.PARAM), newterm, TypeTree(t), nvg)
            val sel = Ident(newterm)

            (sel :: a, newvg :: add, addview, Some(sel))
          } else if (t <:< vwt) {
            val newterm = newTermName(c.fresh("view"))
            val sel = Ident(newterm)
            val newv = ValDef(Modifiers(Flag.PARAM), newterm, TypeTree(t),
              Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil))
            val vgadd: Tree => Tree = addView(_, sel)
            (sel :: a, newv :: add, vgadd :: addview, vg)
          } else if (t =:= act) { // inject a context parameter into arg list if requested
            (ctx.tree :: a, add, addview, vg)
          } else if (t <:< vtt) {
            val ownerTree = if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.owner)
            else if (t.typeSymbol.owner.isClass) This(t.typeSymbol.owner)
            else EmptyTree
            val applyTree = if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.companionSymbol)
            else Select(ownerTree, t.typeSymbol.companionSymbol)
            // WeakTypeTag hack so that nested valdef has correct type
            val tree = inflateAny(ctx, c.Expr(applyTree))(new WeakTypeTag[A] {
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
          sts.reverse ++ List(newvt) ++ vgs.toList.flatMap(v => addviews.reverse.map(_.apply(v))),
          Ident(vtterm)
        )
      )
    }
  }

  val layoutParamFieldOps = Map(
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

  def checkLayoutConstraint(c: Context)(op: String, lpt: c.Type): Unit = {
    import c.universe._
    val lt = layoutType(c, op)
    val vgt = typeOf[ViewGroup]
    val vglpt = typeOf[ViewGroup.LayoutParams]
    val lpc = typeOf[ViewTree.LayoutParamConstraint[_]]
    val lc = typeOf[ViewTree.LayoutConstraint[_]]
    val lc2 = typeOf[ViewTree.LayoutConstraint2[_]]
    val owner = c.macroApplication.children.head.symbol.owner
    val lpcType = owner.typeSignature.baseType(lpc.typeSymbol)
    def typeParamOf(p: Type, base: Type): Type = p.find(_ <:< base).get
    if (lpcType != NoType && !(lpt <:< typeParamOf(lpcType, vglpt))) {
      c.abort(c.macroApplication.pos, s"$op cannot be used in $lpt only in ${typeParamOf(lpcType, vglpt)}")
    }
    val lcType = owner.typeSignature.baseType(lc.typeSymbol)
    val lc2Type = owner.typeSignature.baseType(lc2.typeSymbol)
    val lcs = List(lcType, lc2Type).filter(_ != NoType).map(typeParamOf(_, vgt))
    if (lcs.nonEmpty && !lcs.exists(lt <:< _)) {
      c.abort(c.macroApplication.pos, s"'.$op' cannot be used in $lt only in ${lcs.mkString(" or ")}")
    }
  }

  def withLayoutParams(c: Context)(op: String, lpt: c.Type, stas: List[c.Tree => c.Tree], sts: List[c.Tree]) = {
    import c.universe._
    val defparams = c.Expr[ViewGroup.LayoutParams](defaultLp(c, op))
    val view = c.Expr[View](c.prefix.tree.children.last)
    val checklptype = c.Expr[Boolean](
      TypeApply(Select(
        Apply(Select(view.tree, newTermName("getLayoutParams")), Nil),
        newTermName("isInstanceOf")), List(TypeTree(lpt))))
    val ensureLp = reify {
      if (view.splice.getLayoutParams == null || !checklptype.splice)
        view.splice.setLayoutParams(defparams.splice)
      view.splice.getLayoutParams
    }.tree
    val vterm = newTermName(c.fresh("lp"))
    val vdef = ValDef(Modifiers(Flag.PARAM), vterm, TypeTree(lpt),
      TypeApply(Select(ensureLp, newTermName("asInstanceOf")), List(TypeTree(lpt)))
    )
    c.Expr(Block(
      vdef :: (sts ++ stas.map(_.apply(Ident(vterm)))),
      Literal(Constant(()))
    ))
  }

  def layoutParamField(c: Context)(value: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val (op, lpt) = commonLayoutConstraints(c)
    val intParam: Tree => Tree  = t =>
      Assign(Select(t, newTermName(layoutParamFieldOps(op))), value.tree)
    withLayoutParams(c)(op, lpt, intParam :: Nil, Nil)
  }

  def withTargetApi(c: Context)(tree: c.Expr[Unit]): c.Expr[Unit] = {
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
      }
    }
  }

  def commonLayoutConstraints(c: Context) = {
    val op = c.macroApplication.children.head.symbol.name.encoded
    val lpt = layoutParamType(c, op)
    checkLayoutConstraint(c)(op, lpt)
    (op, lpt)
  }

  def relativeLayoutParamView(c: Context)(view: c.Expr[View]): c.Expr[Unit] = {
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
    withTargetApi(c)(x)
  }

  def relativeLayoutUnary(c: Context)(): c.Expr[Unit] = {
    import c.universe._
    val (op, lpt) = commonLayoutConstraints(c)
    val addRule: Tree => Tree = t =>
      Apply(Select(t, newTermName("addRule")), List(Literal(Constant(relativeLayoutParamViewOps(op))), Literal(Constant(RelativeLayout.TRUE))))
    val x = withLayoutParams(c)(op, lpt, addRule :: Nil, Nil)
    withTargetApi(c)(x)
  }

  def relativeLayoutAlignParent(c: Context)(): c.Expr[Unit] = {
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
    vts.find(_ <:< vgt).get
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
    Apply(Select(
      New(TypeTree(layoutParamType(c, op))), nme.CONSTRUCTOR), args.map(_.tree).toList)
  }

  def lp(c: Context)(args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._
    val view = c.prefix.tree.children.last
    val lp = makeLp(c, "lp")(args:_*)
    c.Expr(Apply(Select(view, newTermName("setLayoutParams")), List(lp)))
  }

  def inflateAny[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[Any]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflateAny(ctx, inflater)
  }
}
