package iota

import android.content.Context
import android.view.View

/**
  * @author pfnguyen
  */
object ViewTreeBoilerplate {
  trait Inflate {
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
    def inflate[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A): A = macro ViewTreeMacro.inflateAny[A]
  }

  trait InflateF {
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
    def inflateF[A <: ViewTree[_]](ctx: Context, inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): A = macro ViewTreeMacro.inflateWithFactory[A]
  }

  trait Check {
    def check[A <: ViewTree[_]](inflater: (_) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
    def check[A <: ViewTree[_]](inflater: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => A)(factory: PartialFunction[String,View]): PartialFunction[String,View] = macro ViewTreeMacro.checkFactory[A]
  }
}
