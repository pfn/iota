package iota.module.macros

import iota.module.macros.Compat210._

/**
 * @author pfnguyen
 */
/**
  * from scalamacros/macrology201/part1/macros/src/main/scala/Macros.scala
  * Internal AST splicing macro, cannot be made private[iota] because it is a nested macro
  */
private[iota] case class OrigOwnerAttachment(sym: Any)
object Splicer {
  import blackbox.Context
  def impl[A](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
    val helper = new Splicer[c.type](c)
    c.Expr[A](helper.changeOwner(expr.tree))
  }
  private class Splicer[C <: Context](val c: C) extends Internal210 {
    def changeOwner(tree: c.Tree): c.Tree = {
      import c.universe._, internal._, decorators._
      val origOwner = tree.attachments.get[OrigOwnerAttachment].map(_.sym).get.asInstanceOf[Symbol]
      c.internal.changeOwner(tree, origOwner, c.internal.enclosingOwner)
    }
  }
  def changeOwner[A](expr: A): A = macro impl[A]
}
