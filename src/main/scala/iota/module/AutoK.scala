package iota.module

import iota.module.macros.AutoKMacro

/**
  * @author pfnguyen
  */
object AutoK extends AutoK
private[iota] trait AutoK {
  import language.dynamics

  /**
    * K-combinator generator, use with any IO container to generate
    * a kestrel automatically, e.g. `w[TextView] >>= k.hint("Hint")` turns into:
    * `w[TextView] >>= kestrel(_.setHint("Hint"))`
    *
    * Another example: `IO(new StringBuilder) >>= k.append("Foo")` turns into:
    * `IO(new StringBuilder) >>= kestrel(_.append("Foo"))`
    *
    * Rules for resolution of the containing object's method are:
    *  `setOnNAMEListener`,
    *  `addOnNAMEListener`,
    *  `setNAMEListener`,
    *  `addNAMEListener`,
    *  `setNAME`,
    *  `NAME`
    */
  object k extends Dynamic {
    def applyDynamic[V,A](method: String)(args: A*): Kestrel[V] = macro AutoKMacro.applyK[V,A]
  }
}

