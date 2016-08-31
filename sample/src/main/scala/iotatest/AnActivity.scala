package iotatest

import java.io.{File, FilenameFilter}

import android.animation.Animator.AnimatorListener
import android.app.Activity
import android.net.ConnectivityManager
import android.os.Bundle
import android.view.{Gravity, View, ViewGroup}
import android.widget._

import scala.concurrent.Future

//import iota.std._
//import Configurations._
//import ViewCombinators._
//import LayoutCombinators._
//import Combinators._
//import Ternary._
//import Contexts._

import iota.effect._
import iota.module.Contexts._
import iota.module.Single._
import iota.module.Configurations._

/**
 * @author pfnguyen
 */
class AnActivity extends Activity {

  import ViewGroup.LayoutParams._
//  IO(new View(this)) >>= lp(MATCH_PARENT,MATCH_PARENT)

  val fl = new FrameLayout(this)
  val weirdMatchP = 2
  val firstlayout = (IO(new FrameLayout(this)) >>= iota.module.ViewCombinatorExtras.gone)(
    IO(new TextView(this)) >>= lp(MATCH_PARENT, weirdMatchP) >>= kestrel { t => t.setText("HI") } >>= id(Id.firsttext),
    IO(new TextView(this)) >>= lpK(MATCH_PARENT, weirdMatchP)((p: ViewGroup.MarginLayoutParams) => 9) >>= k.text("Yo"),
    IO(new TextView(this)) >>= lpK(MATCH_PARENT, weirdMatchP)(margins(all = 5.dp)),
    IO(new TextView(this)) >>= condK(sw(600.dp) ? lp(MATCH_PARENT, MATCH_PARENT) | lp(WRAP_CONTENT, WRAP_CONTENT)),
    IO(new TextView(this)) >>= condK(sw(600.dp) ? lp(WRAP_CONTENT, WRAP_CONTENT))
  )
//  materializeIdType

  import iota.module.MainThreadExecutionContext
  val x = w[TextView] >>= k.visibility(if (true) 1 else 2) >>=
    animate(_.alpha(0)) >>= defer(k.text("Alpha 0")) >>=
    deferF(animate(_.alpha(1))) >>= defer(k.text("Alpha 1"))

  w[Button] >>= k.text("Click Me") >>= hook.onClick { v: View =>
    IO(v.asInstanceOf[TextView]) >>=
      animate(_.alpha(0)) >>= defer(k.text("Alpha 0")) >>=
      deferF(animate(_.alpha(1))) >>= defer(k.text("Alpha 1"))
  }

  w[TextView] >>= k.textAppearance(if (true) new AnActivity else new Foobar, if (true) 1 else 2)

  val tv: TextView = firstlayout.perform().findView(Id.firsttext)
  val tv1: TextView = findView(Id.firsttext)

  IO(new FrameLayout(this))(IO(new View(this)))
//  IO(new View(this)) >>= id(1) >>= hook0.onClick(IO {
//    println("x")
//    "Hi"
//  }) >>= hookM0.onClick.onClick(IO { false }) >>= hook.onClick { (v: View) =>
//    IO(true)
//  } >>= hookM.onClick.onClick((v: View) => IO { v.setOnClickListener(null);  true })
//
//  w[ListView] >>= hookM.scroll.onScrollStateChanged((view: AbsListView, state: Int) => IO {
//    println("state")
//  })

  c[FrameLayout](
    w[TextView] >>= lp(MATCH_PARENT, MATCH_PARENT)
  )

  single[View.OnClickListener] { v: View => println("clicked") }
  single[View.OnClickListener].onClick { v: View => println("clicked") }
  single0[View.OnClickListener].onClick { println("clicked") }
  single0[View.OnClickListener] { println("clicked") }

  new View(this).animate().alpha(1.0f).setListener(single0[AnimatorListener].onAnimationEnd(finish()))

//  findView(Id.iamnotsetyet)
  systemService[ConnectivityManager]
  w[SearchView] >>= hookM.queryText.onQueryTextChange((text: String) => IO {
    false
  })
  w[TextView] >>= id(Id.foobarbaz) >>= hookM.textChanged.onTextChanged((s: CharSequence, start: Int, before: Int, count: Int) => IO {
    false
  })
  w[ImageView] >>= id(Id.foobarbaz)

  val error = 1

  w[TextView] >>= id(android.R.id.background)
  findView(android.R.id.background)
  w[TextView] >>= id(error)

  systemService[android.app.NotificationManager].notify(1, null)
  def f(): Unit = {
//    val f1 = 2
//    w[TextView] >>= id(f1)

//    findView(f1)
//    w[ImageView] >>= id(f1)
    w[ImageView] >>= id(3)
//    w[ImageButton] >>= id(3)

    (findView(3) : ImageView).setImageResource(0)
  }
}

class Frag extends android.app.Fragment {
  materializeContext.getSystemService(android.content.Context.NOTIFICATION_SERVICE)
}

class Foobar extends Activity {

  new Object {
    materializeContext.getSystemService(android.content.Context.NOTIFICATION_SERVICE)
  }
}

class Barbar extends Foobar {
}
class NotFramework extends HasContext {
  def context = null: android.content.Context
  materializeContext.getSystemService(android.content.Context.NOTIFICATION_SERVICE)
}
//class NotFramework2 {
//  def getContext = null: android.content.Context
//  materializeContext.getSystemService(android.content.Context.NOTIFICATION_SERVICE)
//}

object Main extends App {
  implicit class ExtendFiles(val f: File) extends AnyVal {
    def listF(fn: String => Boolean) = f.listFiles(single[FilenameFilter] { (_: File, s: String) => fn(s) })
  }
  val x = IO({ println("First side effect"); "Should appear last" }) >>= kestrel(s => println("Should not see anything before 'Another line'"))

  println("Another line")
  println(x.perform())

  println(Id.foo)
  println(Id.foobar)
  println(Id.foo)

  println((IO(new StringBuilder) >>= k.append("Foo")).perform())

  import scala.concurrent.ExecutionContext.Implicits.global
  def ioSleep[A]: A => IO[Future[A]] = a => IO {
    println("Huh")
    Future {
      Thread.sleep(1000)
      println("Slept")
      a
    }
  }
  def tap[A]: Kestrel[A] = kestrel { a =>
    println("tap: " + a)
  }

  val result =
    IO("hi") >>= tap >>= ioSleep >>= { a => IO {
      a.map(_ + ", there")
    }} >>= defer(tap) >>= { a => IO {
      a.map(_ + ", ok?")
    }} >>= defer(tap) >>=
      deferF(ioSleep) >>= { a => IO {
      println("hm?")
      a.map(_ + " bye!")
    }} >>= defer(tap)
  println("got result: " + result.perform())
  val filter = single[FilenameFilter].accept { (d: File, f: String) => println("HI" + f); true }
  new File("/").listFiles(filter)
  new File("/").listF(s => { println("Found: " + s); true })
}

case class Bugger(container: LinearLayout, text: TextView, b2: foo.Bugger2) extends iota.ViewTree[LinearLayout]
object AnotherTest extends Activity {
  import iota._

  import ViewGroup.LayoutParams._

  override def onCreate(savedInstanceState: Bundle) = super.onCreate(savedInstanceState)

  case class Simple(container: LinearLayout, text: TextView) extends ViewTree[LinearLayout] {
    text.lp(MATCH_PARENT, MATCH_PARENT).lp(WRAP_CONTENT, WRAP_CONTENT).lp(MATCH_PARENT, WRAP_CONTENT)
    text.matchWidth().wrapHeight()
    text.gravity(Gravity.TOP)
    text.marginTop(15)
    text.lp(1,1,1)
  }
  case class TestGrid(container: GridLayout, text: TextView) extends ViewTree[GridLayout] {
    text.lp(GridLayout.spec(1), GridLayout.spec(1))
    text.marginTop(10)

  }
  case class SimpleRelative(ctx: android.content.Context, container: RelativeLayout, text1: TextView = AnotherTest.make[TextView](0), text2: TextView) extends ViewTree[RelativeLayout] {
//    materializeOnClickable[View]
//    Listeners.AnyOnClickable(text1).onClick("foo")

//    Listeners.AnyOnClickable(1).onClick("foo")
//    Listeners.AnyOnClickable("foo").onClick("foo")
    text1.above(text2)
    text1.endOf(text2)
    text1.endOf(text2).alignParentEnd()
    text1.alignParentBottom()
    text1.alignWithParentIfMissing()
    module.Contexts.ViewMaker(ctx).make[Space]
    ctx.make[Space]
    create[Space]
    create[ProgressBar](android.R.attr.progressBarStyleSmall)
    create[ProgressBar]
    nest[FrameLayout](text1) {
      text1.gravity(0)
      if (true) {
        nest[GridLayout](text2) {
          text2.rowSpec(GridLayout.spec(1))
          text2.colSpec(GridLayout.spec(2))
        }
        nest[LinearLayout](text2) {
          text1.gravity(0)
          text1.gravity(0)
          text1.gravity(0)
          text1.gravity(0)
        }
        nest[RelativeLayout](text2) {
        }
      } else {
      }
    }
  }
  val aSimple: Simple = ViewTree.inflate(this, Simple)
  val r = ViewTree.inflate(this, SimpleRelative)
  val pf = ViewTree.check(Nested) {
    case "text" => new TextView(null)
  }
  val r2 = ViewTree.inflateF(this, Nested) {
    case "text"     => new TextView(this)
    case "sub.text" => new TextView(this)
  }
  val r4 = ViewTree.inflateF(this, Nested) {
    case "text" => new TextView(this)
    case "sub.text" => new TextView(this)
    case x => null
  }
  @ViewTree.UnsafeOperation
  val r3 = ViewTree.inflateF(this, Nested)(pf)

  case class NestedItem(container: FrameLayout, text: TextView, sub2: SimpleRelative) extends ViewTree[FrameLayout] {
    text.gravity(Gravity.CENTER)
  }
  case class Nested1(container: LinearLayout, b: Bugger) extends ViewTree[LinearLayout]
  case class Nested(container: LinearLayout, text: TextView = new TextView(AnotherTest), sub: NestedItem) extends ViewTree[LinearLayout]
  val aNested: Nested = ViewTree.inflate(this, Nested)
  val aNested1 = ViewTree.inflate(this, Nested1)
  val bugger = ViewTree.inflate(this, Bugger)
}

package foo {
  case class Bugger2(container: LinearLayout, text: TextView, str: Option[String]) extends iota.ViewTree[LinearLayout]
}

object Foo {
  import iota.module.DefaultExtensions._
//  FooExt.Mat2.materialize[Option[String]]
//  materializeOnClickable[View]
//  materializeOnTouchable[View]
  val view = new View(null)
  val text = new TextView(null)
  text.onClick { () }
  text.onClickEx { view => "" }
  text.onTouchEx { (a, b) => true }
  text.onTouch { true }
}

//object FooExt {
//  import language.experimental.macros
//  object Mat2 {
//    @AndroidTypeclass(List("apply"), "")
//    implicit def materialize[A]: Option[A] = macro ExtensionDefs.materializeTypeclassInstance[Option,A]
//  }
//}

