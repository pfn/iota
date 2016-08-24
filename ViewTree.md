# iota.ViewTree, a new compile-time layout language for Android

ViewTree provides a way to create Android layouts declaratively using
simple Scala data classes with minimal boilerplate and maximal
compile-time and type safety

## Features

1. **[Boilerplate-free layouts](#boilerplate-free-layouts)** - Layouts are declared fully using
   plain Scala `case class`es
2. **[Automatic view "inflation"](#automatic-view-inflation)** - View objects are automatically
   created using default values, custom overrides can be provided for
   styling and theming easily
2. **[Simple and fluent layout extension functions](#simple-and-fluent-layout-extension-functions)** - set `LayoutParams`
   on your views quickly and easily
3. **[Type-safe and null-safe](#type-safe-and-null-safe)** - all layout views are fully known at
   compile-time and completely avoid the `findViewById` of doom
4. **[Fully addressable](#fully-addressable)** - views are completely navigable as they form a
   tree structure of type-safe data classes   
   
### Example

These code examples illustrate creating a layout in XML, and then doing
the same thing in Scala using ViewTree

In XML:

```
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout
        xmlns:android="http://schemas.android.com/apk/res/android"
        android:orientation="vertical"
        android:layout_width="match_parent"
        android:layout_height="match_parent">
    <TextView
        android:layout_height="wrap_content"
        android:layout_width="match_parent"
        android:layout_marginLeft="8dp"
        android:layout_marginTop="8dp"
        android:textAppearance="?android:textAppearanceLarge"
        android:id="@+id/text"
        android:text="Hello world ...."/>
    <ImageView
        android:layout_height="256dp"
        android:layout_width="match_parent"
        android:layout_weight="1"
        android:id="@+id/image"
        android:srcCompat="@drawable/waving_scala_android"
    <LinearLayout
        android:orientation="vertical"
        android:layout_height="wrap_content"
        android:layout_width="wrap_content">
      <ProgressBar
          style="@android:style/Widget.ProgressBar.Small"
          android:layout_width="wrap_content"
          android:layout_width="wrap_content"/>
      <ProgressBar
          style="@android:style/Widget.ProgressBar.Horizontal"
          android:indeterminate="true"
          android:layout_width="wrap_content"
          android:layout_width="wrap_content"/>
      <ProgressBar
          android:layout_width="wrap_content"
          android:layout_width="wrap_content"/>
    </LinearLayout>        
    <LinearLayout
        android:layout_marginBottom="8dp"
        android:layout_marginLeft="8dp"
        android:layout_marginRight="8dp"
        android:layout_height="wrap_content"
        android:layout_width="match_parent">
      <Button
          android:layout_width="wrap_content"
          android:layout_width="wrap_content"
          android:layout_weight="1"
          android:id="@+id/ok"
          android:text="Ok"/>
      <Button
          android:layout_width="wrap_content"
          android:layout_width="wrap_content"
          android:layout_weight="0.5"
          android:id="@+id/cancel"
          android:text="Cancel"/>
    </LinearLayout>
</LinearLayout>
```

In Scala with ViewTree

```
package iota.sample

import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.graphics.drawable.Animatable
import android.widget._
import android.view.ViewGroup.LayoutParams._
import android.view.View.OnClickListener
import iota._

class MainActivity extends AppCompatActivity { self =>
    lazy val views = ViewTree.inflateF(this, Main) {
        case "progress.pb1" =>
            new ProgressBar(this, null, android.R.attr.progressBarStyleSmall)
        case "progress.pb2" =>
            new ProgressBar(this, null, android.R.attr.progressBarStyleHorizontal)
    }
    case class ProgressBars(
        container: LinearLayout,
        pb1: ProgressBar,
        pb2: ProgressBar,
        pb3: ProgressBar
    ) extends ViewTree[LinearLayout] {
        container.setOrientation(LinearLayout.VERTICAL)
        pb2.setIndeterminate(true)
    }
    case class Main(
        container: LinearLayout,
        text: TextView,
        image: ImageView,
        progress: ProgressBars,
        ok: Button,
        cancel: Button
    ) extends ViewTree[LinearLayout] {
        container.setOrientation(LinearLayout.VERTICAL)

        private[this] val buttons = nest[LinearLayout](ok, cancel) {
          ok.lp(WRAP_CONTENT, WRAP_CONTENT, 1)
          cancel.weight(0.5f)
        }.container
        buttons.lp(MATCH_PARENT, WRAP_CONTENT)
        buttons.marginBottom(8.dp)
        buttons.marginLeft(8.dp)
        buttons.marginRight(8.dp)

        text.lp(MATCH_PARENT, WRAP_CONTENT)
        text.marginTop(8.dp)
        text.marginLeft(8.dp)
        text.setText(s"Hello world, from ${TR.string.app_name.value}")
        text.setTextAppearance(self, android.R.style.TextAppearance_Large)

        image.lp(MATCH_PARENT, 256.dp, 1) // can set weight in ctor or separate
        image.weight(1)
        image.setImageResource(R.drawable.waving_scala_android)
        ok.setText("Ok")
        cancel.setText("Cancel")
    }
    
    override def onCreate(savedInstanceState: Bundle): Unit = {
        super.onCreate(savedInstanceState)
        setContentView(views.container)
        views.ok.setOnClickListener(single0[OnClickListener] {
            views.text.setText("I am OK!")
            views.image.getDrawable match {
              case a: Animatable => a.start()
              case _ => // not animatable
            }
        })
        views.cancel.setOnClickListener(single0[OnClickListener] {
            views.text.setText("I have been stopped!")
            views.image.getDrawable match {
              case a: Animatable => a.stop()
              case _ => // not animatable
            }
        })
    }
}
```

The code above results in the following layout:

<image src="http://i.imgur.com/KS7KLR6.png" height="480" width="290">

### Boilerplate-free layouts

Creating a new layout is as simple as creating a new `case class` and
extending `ViewTree[A]`. Any initialization code can be performed
within the body of the class

```
case class SimpleTextLayout(
  container: LinearLayout,
  text: TextView,
  text2: TextView
)
extends ViewTree[LinearLayout]
```

Layouts nest automatically

```
case class ContainerOfText(
  container: FrameLayout,
  progress: ProgressBar,
  simpleText: SimpleTextLayout
) extends ViewTree[FrameLayout] {
  simpleText.container.matchWidth().wrapHeight().gravity(Gravity.BOTTOM)
}
```

`SimpleTextLayout` above gets inflated into `ContainerForText`
automatically when inflating `ContainerForText`. The nesting hierarchy
can be as simple or complex as one wants it to be.

### Automatic view "inflation"

Inflating a view is as simple as:

```
val containerOfText = ViewTree.inflate(anyContext, ContainerOfText)
```

Custom attributes or themes need to be applied? Not a problem!

```
val themedProgressContainerOfText = ViewTree.inflateF(anyContext, ContainerOfText) {
  case "progress" => new ProgressBar(anyContext, null, android.R.attr.progressBarStyleSmall)
}
```

### Simple and fluent layout extension functions

Common layouts are all supported with built-in extension functions to
easily add `LayoutParams` to views.

* common attributes - `lp`, `matchWidth`, `matchHeight`, `wrapWidth`, `wrapHeight`,
  `marginTop`, `marginBottom`, `marginLeft`, `marginRight`
* `LinearLayout` - `weight`, and `gravity`
* `FrameLayout` - `gravity`
* `RelativeLayout` - `above`, `below`, `leftOf`, `rightOf`, `alignLeft`, `alignRight`, and all others

`lp` allows specifying `LayoutParams` constructor parameters directly, when
omitted, defaults of `WRAP_CONTENT` will be used for both width and height.

Layout functions can also be chained in a single expression to save time

```
containerOfText.simpleText.text.matchWidth().marginLeft(8.dp).marginRight(8.dp)
```

### Fully addressable

As seen previously, since everything is a `case class` any view can be
immediately retrieved using standard object navigation.

```
containerOfText.simpleText.text2 // 2nd TextView
containerOfText.simpleText.container // LinearLayout
containerOfText.container // FrameLayout
```

### Type-safe and null-safe

Object navigation is easy as can be with full type-safety, there are no
concerns with missing views as they have been declared and inflated into
the layout classes by compile-time generated code.

> What about that custom view generation in `inflateF`, that looks like it
uses magic strings and can return any type it wants? Won't that possibly
throw runtime exceptions?

No, it doesn't suffer from runtime problems. It is also fully
type-checked. All pattern strings are fully inspected at compile-time
to guarantee that selected paths are correct. Return values are also
inspected at compile-time to ensure conformance to the layout slot into
which they will be inserted.

Lets try an incorrect pattern:

```
val themedProgressContainerOfText = ViewTree.inflateF(anyContext, ContainerOfText) {
  // OOPS! typo of 'progress'
  case "progres" => new ProgressBar(anyContext, null, android.R.attr.progressBarStyleSmall)
}
```

result? no problem!

```
[error] layoutfile.scala:LINE#: not found in class ContainerOfText: pattern 'progress'
[error]     case "progres" => new ProgressBar(anyContext, null, android.R.attr.progressBarStyleSmall)
[error]
```

`inflateF` caught it! How about incorrect return type?

```
val themedProgressContainerOfText = ViewTree.inflateF(anyContext, ContainerOfText) {
  // OOPS! typo of 'progress'
  case "progress" => new TextView(anyContext, null, android.R.attr.progressBarStyleSmall)
}
```

nope!

```
[error] layoutfile.scala:LINE#: ViewTree type mismatch;
[error]  found    : android.widget.TextView
[error]  required : android.widget.ProgressBar
[error]     case "progress" => new TextView(anyContext, null, android.R.attr.progressBarStyleSmall)
```

What if we pass in a partial function symbol instead?

```
val pf: PartialFunction[String,View] = {
  case "progress" => new View(anyContext) // !
}
```

```
[warn] layoutfile.scala:LINE#: Embedded ViewTree factory functions cannot be checked for type-safety
[warn]   ViewTree.inflateF(anyContext, ContainerOfText)(pf)
```

Warnings can be suppressed using `@ViewTree.UnsafeOperation` and in the case
of the broken `pf` above, it can also be checked for safety by using
`ViewTree.check` (also allows automatic type inference for the partial
function as well)

```
val pf = ViewTree.check(ContainerOfText) {
  case "progress" => new View(anyContext) // will now throw a compile time error!
}
```
