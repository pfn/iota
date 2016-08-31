# iota.module.ExtensionDefs, a new way to automatically add android extension functions

ExtensionDefs provides an easy way to easily add extension functions
for Android callbacks with minimal boilerplate, although some
boilerplate is still required.

## Features

1. **[Uses the typeclass pattern](https://tpolecat.github.io/2013/10/12/typeclass.html)** -
   automatically add extensions by creating and/or summoning typeclass instances
2. **[Simple to add extensions](#defining-new-extensions)** - new extension
   functions can be added by defining typeclass traits, and using the macro
   `ExtensionDefs.materializeTypeclassInstance` annotated with
   `@AndroidTypeclass(List("register", "listener", "methods"), "listener callback method")`
3. **[Large set of pre-defined extensions](#including-pre-defined-extensions)** -
   extensions are available for
     * `android.accessibilityservice`
     * `android.animation`
     * `android.app`
     * `android.content`
     * `android.drm`
     * `android.gesture`
     * `android.hardware`
     * `android.location`
     * `android.media`
     * `android.net`
     * `android.os`
     * `android.preference`
     * `android.renderscript`
     * `android.speech`
     * `android.transition`
     * `android.view`
     * `android.webkit`
     * `android.widget`
4. **[Sensible default extensions](#using-default-extensions)** -
   extensions from `android.view` are automatically included when
   importing `iota._` or `iota.module.DefaultExtensions._`. By
   convention, extension functions with an `Ex` suffix handle all of the
   callback's parameters. Without the `Ex` suffix, it is simply called
   as a by-name thunk.
   
### Usage

#### Using default extensions

Default extensions are easy to use, import `iota._` or `iota.module.DefaultExtensions._`
and you're ready to go!

```scala
import iota._
class PerhapsAnActivity extends android.app.Activity {
  val button: Button = ???
  button.onClick {
    finish() // yay, we're done!
  }
  ...
}
```

#### Including pre-defined extensions

Pre-defined extensions are in the `iota.module.extension` package and are
named after the first two parts of the package name, capitalized and
without the `.` separator

```scala
import iota._
object MyExtensions extends iota.module.extension.AndroidContent
  with iota.module.extension.AndroidWidget
  
import android.content._
import MyExtensions._

class PerhapsAnActivity extends android.app.Activity {
  // provided by iota.module.extension.AndroidContent
  val dlg: AlertDialog = ???
  dlg.onShow {
    dlg.getButton(DialogInterface.BUTTON_POSITIVE).onClick {
      // check something before dismissing, common idiom
    }
  }

  // provided by iota.module.extension.AndroidWidget
  val toolbar: Toolbar = ???
  toolbar.onMenuItemClickEx { item => // item is android.view.MenuItem
    doSomethingWith(item) // should be a boolean value
  }
  ...
}
```

#### Defining new extensions

The pre-defined extensions are created on the basis of a very narrow set
of rules. The Listener class must have only a single abstract method,
and the callback method must be named starting with `on`. Sometimes,
we can't use these exact rules, and will want to define our own
extensions, such as adding an `AnimatorListener` to an `Animator` or
a `TextWatcher` to a `TextView`

```scala
import iota.module.AndroidTypeclass
import iota.module.ExtensionDefs
import CustomExtensions._
class PerhapsAnotherActivity extends android.app.Activity {
  val text: TextView = ???
  text.onChange {
    ...
  }
  text.onChangeEx { (s, start, before, count) =>
    ...
  }
  val anim: android.animation.Animator = ???
  anim.onEnd {
    ...
  }
  val anim2: android.view.animation.Animation = ???
  anim2.onEndEx { a =>
    ...
  }
  ...
}

object CustomExtensions extends TextExt with AnimExt
// LIMITATION, typeclass traits cannot be path-dependent!
object ExtTypeclasses {
  trait CanTextViewOnChange[A] {
    def onTextChanged[B](a: A)(handler: => B)
    def onTextChanged2[B](a: A)(handler: (CharSequence, Int, Int, Int) => B)
  }
  trait CanAnimOnEnd[A] {
    def onAnimEnd[B](a: A)(handler: => B)
    def onAnimEnd2[B](a: A)(handler: A => B)
  }
}
trait TextExt {
  import ExtTypeclasses._
  implicit class AnyCanOnChange[A : CanTextViewOnChange](a: A) {
    def onChange[B](handler: => B) = implicitly[CanTextViewOnChange[A]].onTextChanged(a)(handler)
    def onChangeEx[B](handler: (CharSequence, Int, Int, Int) => B) = implicitly[CanTextViewOnChange[A]].onTextChanged2(a)(handler)
  }
  @AndroidTypeclass(List("addTextChangedListener"), "onTextChanged")
  implicit def materializeCanTextViewOnChange[A]: CanTextViewOnChange[A] = macro ExtensionDefs.materializeTypeclassInstance[CanTextViewOnChange,A]
}

// this extension kills 2 birds with 1 stone: Animation and Animator
trait AnimExt {
  import ExtTypeclasses._
  implicit class AnyAnimEnd[A : CanAnimOnEnd](a: A) {
    def onEnd[B](handler: => B) = implicitly[CanAnimOnEnd[A]].onAnimEnd(a)(handler)
    def onEndEx[B](handler: A => B) = implicitly[CanAnimOnEnd[A]].onAnimEnd2(a)(handler)
  }
  @AndroidTypeclass(List("addListener", "setAnimationListener"), "onAnimationEnd")
  implicit def materializeCanAnimOnEnd[A]: CanAnimOnEnd[A] = macro ExtensionDefs.materializeTypeclassInstance[CanAnimOnEnd,A]
}
```
