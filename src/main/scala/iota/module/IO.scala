package iota.module

import android.os.Looper
import android.view.{View, ViewGroup}
import iota.module
import iota.module.macros.IOViewGroupMacro

import scala.concurrent.Future

/** side-effect tracker. call `perform` to execute the side-effects within */
final class IO[+A] private(val perform: () => A) {
  def map[B](f: A => B) = IO(f(perform()))

  /** alias for >>= */
  @inline def flatMap[B](f: A => IO[B]): IO[B] = >>=(f)
  def >>=[B](f: A => IO[B]): IO[B] = IO(f(perform()).perform())

  /** execute side-effects on UI thread */
  def performMain(): Future[A] = Future(perform())(module.MainThreadExecutionContext)
}

object IO {
  def isMainThread = Thread.currentThread == Looper.getMainLooper.getThread
  /** IO factory */
  def apply[A](value: => A): IO[A] = new IO(() => value)
  def sequence[A](ios: Seq[IO[A]]): IO[Seq[A]] = IO(ios.map(_.perform()))

  def perform[A](ios: IO[A]*): Seq[A] = sequence(ios).perform()
  def performMain[A](ios: IO[A]*): Future[Seq[A]] = sequence(ios).performMain()

  implicit class CanAddChild[A <: ViewGroup](val vg: IO[A]) extends AnyVal {
    /** Adds views to an IO[ViewGroup] */
    def apply(body: IO[_ <: View]*): IO[A] = macro IOViewGroupMacro.applyVG[A]
  }
}

