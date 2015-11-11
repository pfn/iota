package iota

import android.view.{View, ViewGroup}

import scala.reflect.macros.Context

/**
 * @author pfnguyen
 */
private[iota] object IOViewGroupMacro {
  def applyVG[A <: ViewGroup : c.WeakTypeTag](c: Context)(body: c.Expr[IO[_ <: View]]*): c.Expr[IO[A]] = {
    val helper = new IOViewGroupMacro[c.type](c)
    helper.applyVG(body)
  }

  private[iota] class IOViewGroupMacro[C <: Context](val c: C) extends Internal210 {
    import c.universe._

    def applyVG[A <: ViewGroup : c.WeakTypeTag](body: Seq[c.Expr[IO[_ <: View]]]): c.Expr[IO[A]] = {
      // drop CanAddView
      val vg = c.prefix.tree.children.tail.head
      applyVGImpl(vg, body)
    }
    def applyVGImpl[A <: ViewGroup : c.WeakTypeTag](vg: Tree, body: Seq[c.Expr[IO[_ <: View]]]): c.Expr[IO[A]] = {
      val transformer = new LpTransformer[c.type](c)
      val lpType = transformer.lpTypeOf(weakTypeOf[A])

      val x: c.Expr[Seq[IO[_ <: View]]] = sequence(c)(
        body.map (b => c.Expr[IO[_ <: View]](transformer.transform(lpType.toType, b.tree)))
      )
      reify {
        IO {
          val iota$generatedContainer$1 = c.Expr[IO[A]](vg).splice.perform()
          x.splice.foreach { iov =>
            val iota$generatedView = iov.perform()
            // ugly workaround for when views are recycled
            if (iota$generatedView.getParent != null)
              iota$generatedView.getParent.asInstanceOf[ViewGroup].removeView(iota$generatedView)
            iota$generatedContainer$1.addView(iota$generatedView)
          }
          iota$generatedContainer$1
        }
      }
    }
  }

  class LpTransformer[C <: Context](val c: C) extends Internal210 {
    import c.universe._
    def lpTypeOf(tpe: Type) = tpe.baseClasses.flatMap { base =>
      val clsname = base.asClass.fullName
      val lpname = clsname + (if (clsname == "android.view.ViewGroup")
        ".MarginLayoutParams" else ".LayoutParams")
      util.Try(rootMirror.staticClass(lpname)).toOption
    }.headOption.getOrElse(
      c.abort(c.enclosingPosition, tpe + " does not have a LayoutParams nested class")
    )
    def isInIota(id: Tree) = {
      id.symbol.owner.name.encoded == "iota" || id.symbol.owner.owner.name.encoded == "iota"
    }
    def transform(lpType: Type, tree: Tree) = {
      val transformer = new Transformer {
        override def transform(tree: c.universe.Tree) = tree match {
          case Apply(n, xs) =>
            n match {
              case Apply(TypeApply(Select(id, f), _), ys) if f.encoded == "lpK" && isInIota(id) =>
                val lp = c.Expr[ViewGroup.LayoutParams](Apply(Select(
                  New(TypeTree(lpType)), nme.CONSTRUCTOR),
                  ys.map(spliceTree(c)(c.internal.enclosingOwner, _))))
                val xbody = spliceTree(c)(c.internal.enclosingOwner, xs.head)
                c.internal.typingTransform(reify {
                  kestrel { (v: View) =>
                    val iota$generatedLayoutParams$1 = lp.splice
                    c.Expr[Any => Any](xbody).splice.apply(iota$generatedLayoutParams$1)
                    v.setLayoutParams(iota$generatedLayoutParams$1)
                  }
                }.tree) { (tree, api) => api.typecheck(tree) }
              case TypeApply(Select(id, f), _) if f.encoded == "lp" && isInIota(id) =>
                val lp = c.Expr[ViewGroup.LayoutParams](Apply(Select(
                  New(TypeTree(lpType)), nme.CONSTRUCTOR),
                  xs.map(spliceTree(c)(c.internal.enclosingOwner, _))))
                c.internal.typingTransform(reify {
                  kestrel { (v: View) =>
                    v.setLayoutParams(lp.splice)
                  }
                }.tree) { (tree, api) => api.typecheck(tree) }
              case _ => super.transform(tree)
            }
          case _ => super.transform(tree)
        }
      }
      transformer.transform(tree)
    }
  }
}
