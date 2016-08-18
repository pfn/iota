package iota

import android.content.{Context => AndroidContext}
import android.view.{View, ViewGroup}

/**
  * @author pfnguyen
  */
trait ViewTree[A <: ViewGroup] {
  val container: A
}
object ViewTree {
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
}

private[iota] object ViewTreeMacro {
  import scala.reflect.macros.Context

  private[iota] class ViewTreeMacro[C <: Context](val c: C) {
    import c.universe._
    private[iota] def inflateAny[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[Any]): c.Expr[A] = {
      val vgt = c.weakTypeOf[ViewGroup]
      val vwt = c.weakTypeOf[View]
      val vtt = c.weakTypeOf[ViewTree[_]]

      val inputs = inflater.tree match {
        case Block(_, Function(in, _)) => in
        case TypeTree() | Select(_, _) | Ident(_) =>
          val applySym = inflater.tree.symbol.typeSignature.member(newTermName("apply")).asMethod
          applySym.paramss.head.map(p => ValDef(Modifiers(Flag.PARAM), newTermName(p.name.toString), TypeTree(p.typeSignature), EmptyTree))
      }
      def addView(vg: Tree, view: Tree): Tree =
        Apply(Select(vg, newTermName("addView")), List(view))
      val (args, sts, addviews, vgs) = inputs.foldLeft(
        (List.empty[Tree], List.empty[Tree], List.empty[Tree => Tree],Option.empty[Tree])) {
        case ((a,add,addview,vg),in) =>
          val t = in.tpt.tpe
          if (in.name.encoded == "container" && t <:< vgt) {
            val nvg = Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil)
            val newterm = c.fresh("viewgroup")
            val newvg = ValDef(Modifiers(Flag.PARAM), newTermName(newterm), TypeTree(t), nvg)
            val sel = Ident(newTermName(newterm))

            (sel :: a, newvg :: add, addview, Some(sel))
          } else if (t <:< vwt) {
            val newterm = c.fresh("view")
            val sel = Ident(newTermName(newterm))
            val newv = ValDef(Modifiers(Flag.PARAM), newTermName(newterm), TypeTree(t),
              Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil))
            val vgadd: Tree => Tree = addView(_, sel)
            (sel :: a, newv :: add, vgadd :: addview, vg)
          } else if (t <:< vtt) {
            val ownerTree = if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.owner)
            else if (t.typeSymbol.owner.isClass) This(t.typeSymbol.owner)
            else EmptyTree
            val applyTree = if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.companionSymbol)
            else Select(ownerTree, t.typeSymbol.companionSymbol)
            val tree = inflateAny(ctx, c.Expr(applyTree))
            val newterm = c.fresh("viewtree")
            val sel = Ident(newTermName(newterm))
            val newvt = ValDef(Modifiers(Flag.PARAM), newTermName(newterm), TypeTree(t), tree.tree)
            val vgadd: Tree => Tree = addView(_, Select(sel, newTermName("container")))

            (sel :: a, newvt :: add, vgadd :: addview, vg)
          } else {
            val container = Option(inflater.tree.symbol).fold("<anon>")(_.fullName)
            c.abort(inflater.tree.pos,
              s"parameter '${in.name}: $t' in $container is not supported,\n" +
                "only android.view.View and iota.ViewTree subclasses are allowed")
          }
      }
      if (vgs.isEmpty) {
        val container = Option(inflater.tree.symbol).fold("<anon>")(_.fullName)
        c.abort(inflater.tree.pos, s"missing 'container' field in $container")
      }
      c.Expr(Block(sts.reverse ++ vgs.toList.flatMap(v => addviews.reverse.map(_.apply(v))),
        Apply(inflater.tree, args.reverse)
      ))
    }
  }
  def inflateAny[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[Any]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflateAny(ctx, inflater)
  }
}
