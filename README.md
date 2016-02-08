# IOTA - an IO Type for Android

A small, an iota, side-effect tracking library and layout DSL

## add to your build

`libraryDependencies += "com.hanhuy.android" %% "iota" % "1.0.0"`

Clear proguard warnings, these will not cause any runtime crashes

`proguardOptions += "-dontwarn iota.**"`

## Usage

`import iota._` or import individual components from `iota.std._` and `iota.IO`

The basic type is `iota.IO` which is a side-effect tracking monad. It offers
the functions `perform`, `performMain`, `map`, `flatMap` and `>>=` which is an
alias for `flatMap`. Any side-effects passed to `IO` will not run until
`perform` is invoked. `IO` objects can be created using the `IO(any)` factory,
and can be composed using `>>=` and `map`

All compositions are functions, no special classes.

### Available components

* `Combinators._` - defines the K-combinator `kestrel[A](f: A => _)`: `A => IO[A]`
* `Kleisli._` - provides `>=>` (andThen) to compose `A => IO[B]` functions
* `Ternary._` - `condK(condition ? (A => IO[B]) | (A => IO[B])) composition`
* `Views._` - has `w[View]`, `l[View]` and `c[View]` functions, `w` and `l`
  behave like in macroid, `w` for creating widgets, and `l` for creating
  ViewGroups with children. `c` is a helper function for providing the ViewGroup
  type for LayoutParams creation when not inside of `IO[_ <: ViewGroup].apply()`.
* `Contexts._` - implicit materializer for `android.content.Context` depending
  on what class one is in (`Activity`, `Fragment`, `HasContext`, etc), also
  implements a type-safe `systemService[T]` lookup
* `Configurations._` - has functions for detecting the current configuration:
  `sw(smallest-width)`, `v(version)`, `landscape` and `portrait`
* `LayoutCombinators._` - has `margins`, `lp` and `lpK` functions for working
  with layout params
* `ViewCombinators._` - generic `View` manipulation, backgrounds, id, `hook*`
  event handlers, `backgroundResource`, etc.
* `TextCombinators._` - `TextView` manipulators, for `text`, `inputType`,
  `hint` and others
* `ImageCombinators._` - set `imageResource` and `imageScale` for `ImageView`s

## Example

The following creates a `LinearLayout` with 2 buttons, "Click Me" and
"Click Me 2", vertically. It also registers onClick listeners to each button
using two different approaches, one where the inputs to the listener are
ignored, and one where the input is used.

```scala
import iota._
import android.app._
import android.os._
import android.content._
import android.view._
import android.widget._

class MyActivity extends Activity {
  // c[LinearLayout] is necessary to hint to `lpK` that LinearLayout.LayoutParams
  // need to be created.
  val button2Adjustments: Kestrel[Button] = c[LinearLayout](
    text("Click Me 2") >=> hook.onClick((v: View) => IO {
      Toast.makeText(this, s"button ${v.getId} was clicked", Toast.LENGTH_SHORT).show()
    }) >=> lpK(WRAP_CONTENT, WRAP_CONTENT, 1.0f){ p: LinearLayout.LayoutParams =>
      p.gravity = Gravity.CENTER
      margins(all = 16.dp)(p)
    }
  )

  val layout = l[LinearLayout](
    w[Button] >>= text("Click Me") >>= hook0.onClick(IO {
      Toast.makeText(this, "The button was clicked", Toast.LENGTH_SHORT).show()
    }) >>= lp(WRAP_CONTENT, WRAP_CONTENT, 1.0f),
    w[Button] >>= button2Adjustments >>= id(Id.button2),
    w[ListView] >>= lp(WRAP_CONTENT, WRAP_CONTENT, 1.0f) >>=
      hookM.scroll.onScrollStateChanged((list: AbsListView, state: Int) => IO {
        // partially implemented listeners with direct method name overrides
        // can also be used as `hookM0.scroll.onScrollStateChanged` if the input
        // parameters can be ignored.
        Toast.makeText(this, "Scroll state changed: " + state, Toast.LENGTH_SHORT).show()
      })
  ) >>= kestrel (_.setOrientation(LinearLayout.VERTICAL))

  // many IDEs can't tell that button2 is a Button, they will often think that
  // this is a View; a type ascription can be used to hint:
  // `lazy val button2: Button = findView(Id.button2)`
  // the type ascribed must be correct or else scalac will crash
  lazy val button2 = findView(Id.button2) // is typed as android.widget.Button

  override def onCreate(b: Bundle) {
    setContentView(layout.perform())
    button2.setText("new label")
  }
}
```

#### Motivations for creation

As a long time user of `macroid` I enjoy what it brought to layout dsl, but do
not like how certain features work: mixing in `Contexts[A]`, super verbose `lp`
layout parameter creation, failures in type inference, lack of type-safety in
ID lookups, and operators that don't necessarily make sense (i.e. `<~`, et al).
The design goal here is to be as pure as possible while remaining terse and
using an existing vocabulary: `kestrel` (K-combinator), `>>=` (flatMap, bind),
`>=>` (kleisli composition), etc.
