package iota.module

/**
  * @author pfnguyen
  */
object Ternary extends TernaryOps {
  implicit class WithTernaryOp(val b: Boolean) extends AnyVal {
    /** ternary expression creator */
    def ?[A](ifTrue: Kestrel[A]): TernaryCondition[A] = TernaryCondition(b, ifTrue)
  }
}

private[iota] trait TernaryOps {
  /** create a K-combinator based on a ternary expression.
    *  for example: `condK(istrue ? (yes => IO[yes]) | (no => IO[no]))`
    */
  def condK[A](ternary: Ternary[A]): Kestrel[A] = ternary match {
    case TernaryCondition(b, ifTrue) => if (b)      ifTrue      else Combinators.noopK[A]
    case TernaryElse(cond, ifFalse)  => if (cond.b) cond.ifTrue else ifFalse
  }
}

sealed trait Ternary[A]
private[iota] case class TernaryElse[A](cond: TernaryCondition[A], ifFalse: Kestrel[A]) extends Ternary[A]
case class TernaryCondition[A] private[iota](b: Boolean, ifTrue: Kestrel[A]) extends Ternary[A] {
  def |(ifFalse: Kestrel[A]): TernaryElse[A] = TernaryElse(this, ifFalse)
}

