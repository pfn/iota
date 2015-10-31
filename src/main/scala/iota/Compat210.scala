package iota

/**
 * @author pfnguyen
 */

// defines stubs for stuff that's missing in Scala 2.10
object Compat210 {
  object blackbox { // scala.reflect.macros.blackbox package
  type Context = scala.reflect.macros.Context
  }
  object internal // Context.internal object
  object decorators // Context.decorators object
  object contexts // scala.reflect.macros.contexts package
}
import Compat210._

// unifies scala.reflect.macros.runtime.Context (Scala 2.10)
// and scala.reflect.macros.contexts.Context (Scala 2.11)
object Power {
  import scala.reflect.macros._
  object DummyScope
  {
    import runtime._
    import contexts._
    type Result = Context
  }
  type PowerContext = DummyScope.Result
}
import Power._

// a cake slice that can be mixed into improvised macro bundles
// to transparently bring new Scala 2.11 features to Scala 2.10
trait Internal210 { self =>
  import scala.reflect.macros._
  import blackbox.Context

  val c: Context
  import c.universe._

  // enrichments that backport parts of Scala 2.11's c.internal to Scala 2.10
  // these are only going to be picked up if we compile against Scala 2.10
  implicit class RichContext(val c: self.c.type) {
    object internal {
      def enclosingOwner: Symbol = {
        val powerContext = c.asInstanceOf[PowerContext]
        powerContext.callsiteTyper.context.owner.asInstanceOf[Symbol]
      }
      def changeOwner(tree: Tree, oldOwner: Symbol, newOwner: Symbol): Tree = {
        val powerContext = c.asInstanceOf[PowerContext]
        val global = powerContext.universe
        class ChangeOwnerAndModuleClassTraverser(oldOwner: global.Symbol, newOwner: global.Symbol) extends global.ChangeOwnerTraverser(oldOwner, newOwner) {
          override def traverse(tree: global.Tree) {
            tree match {
              case _: global.DefTree => change(tree.symbol.moduleClass)
              case _ =>
            }
            super.traverse(tree)
          }
        }
        val traverser = new ChangeOwnerAndModuleClassTraverser(oldOwner.asInstanceOf[global.Symbol], newOwner.asInstanceOf[global.Symbol])
        traverser.traverse(tree.asInstanceOf[global.Tree])
        tree
      }
      def valDef(sym: Symbol, rhs: Tree): ValDef = {
        val powerContext = c.asInstanceOf[PowerContext]
        val global = powerContext.universe
        global.ValDef(sym.asInstanceOf[global.Symbol], rhs.asInstanceOf[global.Tree]).asInstanceOf[ValDef]
      }
      trait TypingTransformApi {
        def default(tree: Tree): Tree
        def typecheck(tree: Tree): Tree
      }
      def typingTransform(tree: Tree)(transformer: (Tree, TypingTransformApi) => Tree): Tree = {
        val powerContext = c.asInstanceOf[PowerContext]
        val global = powerContext.universe
        class MiniCake[G <: scala.tools.nsc.Global](val global: G) extends scala.tools.nsc.transform.TypingTransformers {
          val callsiteTyper = powerContext.callsiteTyper.asInstanceOf[global.analyzer.Typer]
          class HofTypingTransformer(hof: (Tree, TypingTransformApi) => Tree) extends TypingTransformer(callsiteTyper.context.unit) { self =>
            currentOwner = callsiteTyper.context.owner
            curTree = global.EmptyTree
            localTyper = global.analyzer.newTyper(callsiteTyper.context.make(unit = callsiteTyper.context.unit))
            val api = new TypingTransformApi {
              def default(tree: Tree): Tree = superTransform(tree.asInstanceOf[global.Tree]).asInstanceOf[Tree]
              def typecheck(tree: Tree): Tree = localTyper.typed(tree.asInstanceOf[global.Tree]).asInstanceOf[Tree]
            }
            def superTransform(tree: global.Tree) = super.transform(tree)
            override def transform(tree: global.Tree): global.Tree = hof(tree.asInstanceOf[Tree], api).asInstanceOf[global.Tree]
          }
        }
        val miniCake = new MiniCake[global.type](global)
        new miniCake.HofTypingTransformer(transformer).transform(tree.asInstanceOf[global.Tree]).asInstanceOf[Tree]
      }
    }
  }

  // we can't use Symbol.setInfo in Scala 2.10 (doesn't exist yet) or Symbol.setTypeSignature in Scala 2.11 (has been removed)
  // therefore we need to settle on some sort of a middle ground
  implicit class RichSymbol(val sym: self.c.universe.Symbol) {
    def setInfoCompat(info: Type): Symbol = {
      import compat._
      sym.setTypeSignature(info)
    }
  }
}

package macroutil {

import language.experimental.macros

case class OrigOwnerAttachment(sym: Any)

object Splicer {

  import blackbox.Context

  def impl[A](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
    val helper = new Splicer[c.type](c)
    c.Expr[A](helper.changeOwner(expr.tree))
  }

  def changeOwner[A](expr: A): A = macro impl[A]
}

class Splicer[C <: reflect.macros.Context with Singleton](val c: C) extends Internal210 {
  /** Safely splice the tree `t` into the enclosing lexical context in which it will
    * be type checked. Rather than directly include `t` in the result, a layer of
    * indirection is used: a call to the macro `changeOwner`.
    *
    * This macro is provied with the symbol of the current enclosing context
    * (`c.enclosingOwner`), and the tree `t`.
    *
    * When it is typechecked, it will have access to another macro context with
    * a new enclosing owner. This is substituted from the old owner.
    *
    * This avoids the tedium of manually creating symbols for synthetic enclosing
    * owners when splicing macro arguments. And it avoids the bugs that still plaugue
    * `untypecheck` (e.g. https://issues.scala-lang.org/browse/SI-8500)
    *
    * This approach only works in cases when you are splicing the arguments into leaf
    * position in the synthetic tree. If you need to splice typed trees *above* untyped
    * trees, it will fail because the typecheck stops descending when it finds a typed
    * tree.
    */

  def changeOwner(tree: c.Tree): c.Tree = {
    import c.universe._
    val origOwner = tree.attachments.get[OrigOwnerAttachment].map(_.sym).fold(c.weakTypeOf[Nothing].typeSymbol)(_.asInstanceOf[Symbol])
    c.internal.changeOwner(tree, origOwner, c.internal.enclosingOwner)
  }

}

}
