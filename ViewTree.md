# iota.ViewTree, a new compile-time layout language for Android

ViewTree provides a way to create Android layouts declaratively using
simple Scala data classes with minimal boilerplate and maximal
compile-time and type safety

## Features

1. **Boilerplate-free layouts** - Layouts are declared fully using
   plain Scala `case class`es
2. **Automatic view "inflation"** - View objects are automatically
   created using default values, custom overrides can be provided for
   styling and theming easily
2. **Simple and fluent layout extension functions** - set `LayoutParams`
   on your views quickly and easily
3. **Type-safe and null-safe** - all layout views are fully known at
   compile-time and completely avoid the `findViewById` of doom
4. **Fully addressable** - views are fully navigable as they form a
   tree structure of type-safe data classes   
   
### Example

This gist illustrates the layout in XML, and then in Scala using ViewTree

<script src="https://gist.github.com/pfn/e82a9c11d0bd5761837037eabe6a2685.js"></script>

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
