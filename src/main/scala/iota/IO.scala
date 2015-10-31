package iota

import android.os.{Handler, Looper}
import android.util.Log
import android.view.{View, ViewGroup}

import scala.reflect.internal.annotations.compileTimeOnly
import scala.reflect.macros.{Context => MacroContext, TypecheckException}
import language.experimental.macros

import scala.concurrent.{Future, ExecutionContext}

class IO[+A] private(value: () => A) {
  def map[B](f: A => B) = IO(f(value()))
  def >>=[B](f: A => IO[B]): IO[B] = IO(f(value()).performIO())

  def performIO(): A = value()
  def mainThreadIO(): Future[A] = Future(value())(MainThreadExecutionContext)
}

object IO {
  def isMainThread = Thread.currentThread == Looper.getMainLooper.getThread
  def apply[A](value: => A): IO[A] = new IO(() => value)
  def sequence[A](ios: Seq[IO[A]]): IO[Seq[A]] = IO(ios.map(_.performIO()))
}

object MainThreadExecutionContext extends ExecutionContext {
  private[this] lazy val handler = new Handler(Looper.getMainLooper)
  override def execute(runnable: Runnable) = handler.post(runnable)
  override def reportFailure(t: Throwable) = Log.e("IOTA", t.getMessage, t)
}

object Combinators {
  type IOCombinator[A] = A => IO[A]

  /** K-combinator */
  def kestrel[A,B](f: A => B): IOCombinator[A] = a => IO { f(a); a }

  def id[A <: View](id: Int): IOCombinator[A] = kestrel((_: A).setId(id))

  def visibility[A <: View](visibility: Int): IOCombinator[A] =
    kestrel((_: A).setVisibility(visibility))
  def gone[A <: View]:      IOCombinator[A] = visibility(View.GONE)
  def visible[A <: View]:   IOCombinator[A] = visibility(View.VISIBLE)
  def invisible[A <: View]: IOCombinator[A] = visibility(View.INVISIBLE)

  def enabled[A <: View](enable: Boolean): IOCombinator[A] =
    kestrel((_: A).setEnabled(enable))
  def enabled[A <: View]:  IOCombinator[A] = enabled(true)
  def disabled[A <: View]: IOCombinator[A] = enabled(false)

  def sequence[A](c: MacroContext)(xs: Seq[c.Expr[A]]): c.Expr[Seq[A]] = {
    import c.universe._
    c.Expr(Apply(Select(reify(Seq).tree, newTermName("apply")),
      xs.map(_.tree).toList
    ))
  }

  implicit class CanAddChild[A <: ViewGroup](val vg: IO[A]) extends AnyVal {
    def apply[V <: View](body: IO[V]*): IO[A] = macro IOViewGroupMacros.applyVG[A,V]
  }

  @inline
  def margins(top: Int = 0, left: Int = 0, right: Int = 0, bottom: Int = 0, all: Int = -1): ViewGroup.MarginLayoutParams => Unit = lp => {
    if (all != -1) {
      lp.topMargin = all
      lp.leftMargin = all
      lp.rightMargin = all
      lp.bottomMargin = all
    } else {
      lp.topMargin = top
      lp.leftMargin = left
      lp.rightMargin = right
      lp.bottomMargin = bottom
    }
  }
  @compileTimeOnly("lp can only be used from IO[_ <: ViewGroup].apply()")
  def lp[V <: View](args: Any*): IOCombinator[V] = ???

  @compileTimeOnly("lpK can only be used from IO[_ <: ViewGroup].apply()")
  /** K-combinator for LayoutParams */
  def lpK[V <: View,A,B](args: Any*)(k: A => B): IOCombinator[V] = ???
}

import Compat210._
object IOViewGroupMacros {
  def applyVG[A <: ViewGroup : c.WeakTypeTag, V <: View : c.WeakTypeTag](c: MacroContext)(body: c.Expr[IO[V]]*): c.Expr[IO[A]] = {
    val helper = new IOViewGroupMacros[c.type](c)
    helper.applyVG(body)
  }

  class IOViewGroupMacros[C <: MacroContext](val c: C) extends Internal210 {

    def spliceTree(t: c.Tree): c.Tree = {
      import c.universe._
      //     smuggle the symbol of the current enclosing owner through to the
      //     `changeOwner` as the symbol of the tree of its first argument.
      //     Tree attachments would be a more principled approach, but they aren't
      //     part of the public API.
      t.updateAttachment(macroutil.OrigOwnerAttachment(c.internal.enclosingOwner))
//      reify { macroutil.Splicer.changeOwner(t) }
      Apply(Select(reify(macroutil.Splicer).tree, newTermName("changeOwner")), t :: Nil)
    }

    def sequence[A](xs: Seq[c.Expr[A]]): c.Expr[Seq[A]] = {
      import c.universe._
      c.Expr(Apply(Select(reify(Seq).tree, newTermName("apply")),
        xs.map(_.tree).toList
      ))
    }
    def applyVG[A <: ViewGroup : c.WeakTypeTag, V <: View : c.WeakTypeTag](body: Seq[c.Expr[IO[V]]]): c.Expr[IO[A]] = {
      import c.universe._
      val vg = c.prefix.tree.collect {
        case a@Apply(n, xs) => xs
      }.flatten.apply(1)

      val lpType = c.weakTypeTag[A].tpe.baseClasses.flatMap(base =>
        util.Try(rootMirror.staticClass(base.asClass.fullName + ".LayoutParams")).toOption
      ).headOption.getOrElse(
        c.abort(c.enclosingPosition, c.weakTypeOf[A] + " does not have a LayoutParams nested class")
      )

      final class MessageOnlyException(override val toString: String)
        extends RuntimeException(toString) {
        override def getStackTrace = Array.ofDim(0)
      }


      val transformer = new Transformer {
        override def transform(tree: c.universe.Tree) = tree match {
          case Apply(n, xs) =>
            n match {
              case Apply(TypeApply(Select(id, f), _), ys) if id.tpe =:= typeOf[Combinators.type] && f.encoded == "lpK" =>
                val lp = c.Expr[ViewGroup.LayoutParams](Apply(Select(New(TypeTree(lpType.toType)), nme.CONSTRUCTOR), ys))
                val xbody = spliceTree(xs.head)
                try {
                  c.internal.typingTransform(reify {
                    Combinators.kestrel { (v: V) =>
                      val iota$generatedLayoutParams$1 = lp.splice
                      c.Expr[Any => Any](xbody).splice.apply(iota$generatedLayoutParams$1)
                      v.setLayoutParams(iota$generatedLayoutParams$1)
                    }
                  }.tree) { (tree, api) => api.typecheck(tree) }
                } catch {
                  case t: TypecheckException =>
                    throw new MessageOnlyException(t.getMessage)
                }
              case TypeApply(Select(id, f), _) if id.tpe =:= typeOf[Combinators.type] && f.encoded == "lp" =>
                val lp = c.Expr[ViewGroup.LayoutParams](Apply(Select(New(TypeTree(lpType.toType)), nme.CONSTRUCTOR), xs.map(spliceTree)))
                c.internal.typingTransform(reify {
                  Combinators.kestrel { (v: V) =>
                    v.setLayoutParams(lp.splice)
                  }
                }.tree) { (tree, api) => api.typecheck(tree) }
              case _ => super.transform(tree)
            }
          case _ => super.transform(tree)
        }

      }
      val body2 = body.map { b =>
        transformer.transform(b.tree)
      }.map(c.Expr[IO[V]](_))
      val x = sequence(body2)
      val expr = reify {
        IO {
          val container = c.Expr[A](vg).splice
          x.splice.foreach { iov => container.addView(iov.performIO()) }
          container
        }
      }
      expr
    }

  }
}
