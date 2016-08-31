package iota.module

import iota.module.macros.ExtensionDefsMacro

/**
  * @author pfnguyen
  */

object ExtensionDefs {
  def materializeTypeclassInstance[C[_],A : c.WeakTypeTag](c: reflect.macros.Context)(implicit ctag: c.WeakTypeTag[C[A]]): c.Expr[C[A]] =
    ExtensionDefsMacro.materializeTypeclassInstance(c)(implicitly[c.WeakTypeTag[A]], ctag)
}
