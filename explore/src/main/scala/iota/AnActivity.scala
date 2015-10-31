package iota

import android.app.Activity

import Contexts._
import Combinators._
import android.view.{ViewGroup, View}
import android.widget._
import Resources._
import Contexts._

/**
 * @author pfnguyen
 */
class AnActivity extends Activity {

  import ViewGroup.LayoutParams._
//  IO(new View(this)) >>= lp(MATCH_PARENT,MATCH_PARENT)
  val fl = new FrameLayout(this)
//  implicit val x = LayoutParamsContext[FrameLayout]()
//  implicit val x = materializeLayoutParamsContext[FrameLayout]

  val weirdMatchP = 2
  IO(new FrameLayout(this))(
    IO(new TextView(this)) >>= lp(MATCH_PARENT, weirdMatchP) >>= kestrel { t => t.setText("HI") },
    IO(new TextView(this)) >>= lpK(MATCH_PARENT, weirdMatchP)(margins(all = 5.dp))
  )
  IO(new FrameLayout(this))(IO(new View(this)))
  IO(new View(this)) >>= Combinators.id(1)

  1.dp
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
class NotFramework extends WithContext {
  def getContext = null: android.content.Context
  materializeContext.getSystemService(android.content.Context.NOTIFICATION_SERVICE)
}
//class NotFramework2 {
//  def getContext = null: android.content.Context
//  materializeContext.getSystemService(android.content.Context.NOTIFICATION_SERVICE)
//}

object Main extends App {
  val x = IO({ println("First side effect"); "Foo" }) >>= kestrel(s => println("Should not see this before 'Another line': " + s))

  println("Another line")
  println(x.performIO())
}
