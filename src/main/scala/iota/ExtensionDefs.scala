package iota

import android.view.View

import scala.annotation.implicitNotFound

/**
  * @author pfnguyen
  */

class AndroidTypeclass(register: List[String], callback: String) extends annotation.StaticAnnotation

trait AndroidExtensions extends Any {
  @AndroidTypeclass(List("addOnClickListener", "setOnClickListener"), "onClick")
  implicit def materializeOnClickable[A]: OnClickable[A] = macro ExtensionDefs.materializeTypeclassInstance[OnClickable,A]
  @AndroidTypeclass(List("addTextChangedListener"), "onTextChanged")
  implicit def materializeOnTextChangeable[A]: OnTextChangeable[A] = macro ExtensionDefs.materializeTypeclassInstance[OnTextChangeable,A]
  @AndroidTypeclass(List("setOnTouchListener"), "onTouch")
  implicit def materializeOnTouchable[A]: OnTouchable[A] = macro ExtensionDefs.materializeTypeclassInstance[OnTouchable,A]

  @implicitNotFound("Could not find a way to add onClick to ${A}")
  trait OnClickable[A] extends Any {
    def onClick[B](v: A, handler: => B)
    def onClick[B](v: A, handler: View => B)
  }
  @implicitNotFound("Could not find a way to add onTextChanged to ${A}")
  trait OnTextChangeable[A] extends Any {
    def onTextChanged[B](v: A, handler: (CharSequence, Int, Int, Int) => B)
    def onTextChanged[B](v: A, handler: => B)
  }
  @implicitNotFound("Could not find a way to add onTouch to ${A}")
  trait OnTouchable[A] extends Any {
    def onTouchEx(v: A, handler: => Boolean)
    def onTouch(v: A, handler: (View, android.view.MotionEvent) => Boolean)
  }

  implicit class AnyOnClickable[A : OnClickable](val a: A) {
    def onClick[B](handler: => B): Unit = implicitly[OnClickable[A]].onClick(a, handler)
    def onClickEx[B](handler: A => B): Unit = implicitly[OnClickable[A]].onClick(a, handler)
  }

  implicit class AnyWatchableTextEx[A : OnTextChangeable](val a: A) {
    def onTextChangedEx[B](handler: (CharSequence, Int, Int, Int) => B): Unit = implicitly[OnTextChangeable[A]].onTextChanged(a, handler)
    def onTextChange[B](handler: => B): Unit = implicitly[OnTextChangeable[A]].onTextChanged(a, handler)
  }

  implicit class AnyOnTouchable[A : OnTouchable](val a: A) {
    def onTouchEx(handler: (View, android.view.MotionEvent) => Boolean): Unit = implicitly[OnTouchable[A]].onTouch(a, handler)
    def onTouch(handler:  => Boolean): Unit = implicitly[OnTouchable[A]].onTouchEx(a, handler)
  }
}
object ExtensionDefs {
  def materializeTypeclassInstance[C[_],A : c.WeakTypeTag](c: reflect.macros.Context)(implicit ctag: c.WeakTypeTag[C[A]]): c.Expr[C[A]] =
    ExtensionDefsMacro.materializeTypeclassInstance(c)(implicitly[c.WeakTypeTag[A]], ctag)
}
