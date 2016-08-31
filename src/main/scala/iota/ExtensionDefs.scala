package iota

/**
  * @author pfnguyen
  */

class AndroidTypeclass(register: List[String], callback: String) extends annotation.StaticAnnotation

object ExtensionDefs {
  def materializeTypeclassInstance[C[_],A : c.WeakTypeTag](c: reflect.macros.Context)(implicit ctag: c.WeakTypeTag[C[A]]): c.Expr[C[A]] =
    ExtensionDefsMacro.materializeTypeclassInstance(c)(implicitly[c.WeakTypeTag[A]], ctag)
}
