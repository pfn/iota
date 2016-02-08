package iotatest

import java.io.{File, FilenameFilter}

import android.animation.Animator.AnimatorListener
import android.app.Activity
import android.net.ConnectivityManager

import android.view.{ViewGroup, View}
import android.widget._
//import iota.std._
//import Configurations._
//import ViewCombinators._
//import LayoutCombinators._
//import Combinators._
//import Ternary._
//import Contexts._

import iota._
//import iota.IO
//import iota.WithContext

/**
 * @author pfnguyen
 */
class AnActivity extends Activity {

  import ViewGroup.LayoutParams._
//  IO(new View(this)) >>= lp(MATCH_PARENT,MATCH_PARENT)

  val fl = new FrameLayout(this)
  val weirdMatchP = 2
  val firstlayout = (IO(new FrameLayout(this)) >>= gone)(
    IO(new TextView(this)) >>= lp(MATCH_PARENT, weirdMatchP) >>= kestrel { t => t.setText("HI") } >>= id(Id.firsttext),
    IO(new TextView(this)) >>= lpK(MATCH_PARENT, weirdMatchP)((p: ViewGroup.MarginLayoutParams) => 9),
    IO(new TextView(this)) >>= lpK(MATCH_PARENT, weirdMatchP)(margins(all = 5.dp)),
    IO(new TextView(this)) >>= condK(sw(600.dp) ? lp(MATCH_PARENT, MATCH_PARENT) | lp(WRAP_CONTENT, WRAP_CONTENT)),
    IO(new TextView(this)) >>= condK(sw(600.dp) ? lp(WRAP_CONTENT, WRAP_CONTENT))
  )
//  materializeIdType

  val tv: TextView = firstlayout.perform().findView(Id.firsttext)
  val tv1: TextView = findView(Id.firsttext)

  IO(new FrameLayout(this))(IO(new View(this)))
  IO(new View(this)) >>= id(1) >>= hook0.onClick(IO {
    println("x")
    "Hi"
  }) >>= hookM0.onClick.onClick(IO { false }) >>= hook.onClick { (v: View) =>
    IO(true)
  } >>= hookM.onClick.onClick((v: View) => IO { v.setOnClickListener(null);  true })

  w[ListView] >>= hookM.scroll.onScrollStateChanged((view: AbsListView, state: Int) => IO {
    println("state")
  })

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
    val f1 = 2
    w[TextView] >>= id(f1)

    findView(f1)
    w[ImageView] >>= id(f1)
    w[ImageView] >>= id(3)
    w[ImageButton] >>= id(3)

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
  val x = IO({ println("First side effect"); "Should appear last" }) >>= kestrel(s => println("Should not see anything before 'Another line'"))

  println("Another line")
  println(x.perform())

  println(Id.foo)
  println(Id.foobar)
  println(Id.foo)

  println((IO(new StringBuilder) >>= k.append("Foo")).perform())

  val filter = single[FilenameFilter].accept { (d: File, f: String) => println("HI" + f); true }
  new File("/").listFiles(filter)
}
