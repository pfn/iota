package iota.module

import iota.module.macros.ExtensionDefsMacro

/**
  * @author pfnguyen
  */
object ExtensionDefs {
  /**
    * Used to define new summon typeclass instances, like so:
    * {{{
    * object CustomExtensions extends TextExt
    * // LIMITATION, typeclass traits cannot be path-dependent!
    * object ExtTypeclasses {
    *   trait CanTextViewOnChange[A] {
    *     def onTextChanged[B](a: A)(handler: => B)
    *     def onTextChanged2[B](a: A)(handler: (CharSequence, Int, Int, Int) => B)
    *   }
    * }
    * trait TextExt {
    *   import ExtTypeclasses._
    *   implicit class AnyCanOnChange[A : CanTextViewOnChange](a: A) {
    *     def onChange[B](handler: => B) = implicitly[CanTextViewOnChange[A]].onTextChanged(a)(handler)
    *     def onChangeEx[B](handler: (CharSequence, Int, Int, Int) => B) = implicitly[CanTextViewOnChange[A]].onTextChanged2(a)(handler)
    *   }
    *   @AndroidTypeclass(List("addTextChangedListener"), "onTextChanged")
    *   implicit def materializeCanTextViewOnChange[A]: CanTextViewOnChange[A] = macro ExtensionDefs.materializeTypeclassInstance[CanTextViewOnChange,A]
    * }
    * }}}
    */
  def materializeTypeclassInstance[C[_],A : c.WeakTypeTag](c: reflect.macros.Context)(implicit ctag: c.WeakTypeTag[C[A]]): c.Expr[C[A]] =
    ExtensionDefsMacro.materializeTypeclassInstance(c)(implicitly[c.WeakTypeTag[A]], ctag)
}
