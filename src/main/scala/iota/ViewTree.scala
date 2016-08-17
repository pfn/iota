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
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _) => A): A = macro ViewTreeMacro.inflate2[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _) => A): A = macro ViewTreeMacro.inflate3[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _) => A): A = macro ViewTreeMacro.inflate4[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _) => A): A = macro ViewTreeMacro.inflate5[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate6[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate7[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate8[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate9[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate10[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate11[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate12[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate13[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate14[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate15[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate16[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate17[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate18[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate19[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate20[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate21[A]
  def inflate[A <: ViewTree[_]](ctx: AndroidContext, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflate22[A]
}

private[iota] object ViewTreeMacro {
  import scala.reflect.macros.Context

  private[iota] class ViewTreeMacro[C <: Context](val c: C) {
    import c.universe._
    private[this] def inflateAll[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[Any]): c.Expr[A] = {
      val vgt = c.weakTypeOf[ViewGroup]
      val vwt = c.weakTypeOf[View]
      val vtt = c.weakTypeOf[ViewTree[_]]

      val inputs = inflater.tree match {
        case Block(_, Function(in, _)) => in
        case TypeTree() | Select(_, _) | Ident(_) =>
          val applySym = inflater.tree.symbol.typeSignature.member(newTermName("apply")).asMethod
          applySym.paramss.head.map(p => ValDef(Modifiers(Flag.PARAM), newTermName(p.name.toString), TypeTree(p.typeSignature), EmptyTree))
      }
      val (args, sts, _) = inputs.zipWithIndex.foldLeft(
        (List.empty[Tree], List.empty[Tree], Option.empty[Tree])) {
        case ((a,add,vg),(in, i)) =>
          val t = in.tpt.tpe
          if (i == 0 && t <:< vgt) {
            val nvg = Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil)
            val newterm = c.fresh("viewgroup")
            val newvg = ValDef(Modifiers(Flag.PARAM), newTermName(newterm), TypeTree(t), nvg)
            val sel = Ident(newTermName(newterm))

            (sel :: a, newvg :: add, Some(sel))
          } else if (t <:< vwt) {
            val newterm = c.fresh("view")
            val sel = Ident(newTermName(newterm))
            val newv = ValDef(Modifiers(Flag.PARAM), newTermName(newterm), TypeTree(t),
              Apply(Select(New(TypeTree(t)), nme.CONSTRUCTOR), ctx.tree :: Nil))
            val vgadd = vg.toList.map { v =>
              Apply(Select(v, newTermName("addView")), List(sel))
            }
            (sel :: a, vgadd ++ (newv :: add), vg)
          } else if (t <:< vtt) {
            val ownerTree = if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.owner)
            else if (t.typeSymbol.owner.isClass) This(t.typeSymbol.owner)
            else EmptyTree
            val applyTree = if (t.typeSymbol.owner.isPackage) Ident(t.typeSymbol.companionSymbol)
            else Select(ownerTree, t.typeSymbol.companionSymbol)
            val tree = inflateAll(ctx, c.Expr(applyTree))
            val newterm = c.fresh("viewtree")
            val sel = Ident(newTermName(newterm))
            val newvt = ValDef(Modifiers(Flag.PARAM), newTermName(newterm), TypeTree(t), tree.tree)
            val vgadd = vg.toList.map { v =>
              Apply(Select(v, newTermName("addView")), List(Select(sel, newTermName("container"))))
            }

            (sel :: a, vgadd ++ (newvt :: add), vg)
          } else {
            c.abort(in.pos,
              s"ViewTree parameter '${in.name}: $t' is not supported,\n" +
                "only android.view.View and iota.ViewTree subclasses are allowed")
          }
      }
      c.Expr(Block(sts.reverse,
        Apply(inflater.tree, args.reverse)
      ))
    }
    def inflate2[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate3[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate4[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate5[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate6[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate7[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate8[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate9[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate10[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate11[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate12[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate13[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate14[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate15[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate16[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate17[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate18[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate19[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate20[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate21[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
    def inflate22[A: c.WeakTypeTag](ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = inflateAll(ctx, inflater)
  }
  def inflate2[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate2(ctx, inflater)
  }
  def inflate3[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate3(ctx, inflater)
  }
  def inflate4[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate4(ctx, inflater)
  }
  def inflate5[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate5(ctx, inflater)
  }
  def inflate6[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate6(ctx, inflater)
  }
  def inflate7[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate7(ctx, inflater)
  }
  def inflate8[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate8(ctx, inflater)
  }
  def inflate9[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate9(ctx, inflater)
  }
  def inflate10[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate10(ctx, inflater)
  }
  def inflate11[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate11(ctx, inflater)
  }
  def inflate12[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate12(ctx, inflater)
  }
  def inflate13[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate13(ctx, inflater)
  }
  def inflate14[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate14(ctx, inflater)
  }
  def inflate15[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate15(ctx, inflater)
  }
  def inflate16[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate16(ctx, inflater)
  }
  def inflate17[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate17(ctx, inflater)
  }
  def inflate18[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate18(ctx, inflater)
  }
  def inflate19[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate19(ctx, inflater)
  }
  def inflate20[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate20(ctx, inflater)
  }
  def inflate21[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate21(ctx, inflater)
  }
  def inflate22[A: c.WeakTypeTag](c: Context)(ctx: c.Expr[AndroidContext], inflater: c.Expr[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A]): c.Expr[A] = {
    val helper = new ViewTreeMacro[c.type](c)
    helper.inflate22(ctx, inflater)
  }
}
