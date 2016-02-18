package iota

import android.os.{Looper, Handler}
import android.util.Log

import scala.concurrent.ExecutionContext

/**
  * @author pfnguyen
  */
package object std {

  /** UI execution context, use with combinators such as `defer` and `deferF`
    */
  implicit object MainThreadExecutionContext extends ExecutionContext {
    private[this] lazy val handler = new Handler(Looper.getMainLooper)
    override def execute(runnable: Runnable) = handler.post(runnable)
    override def reportFailure(t: Throwable) = Log.e("IOTA", t.getMessage, t)
  }
}