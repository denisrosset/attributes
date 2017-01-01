package net.alasc.attributes

object Compat {

  type Context = scala.reflect.macros.whitebox.Context

  type MutableLongMap[A] = scala.collection.mutable.LongMap[A]

  def emptyMutableLongMap[A]: MutableLongMap[A] = scala.collection.mutable.LongMap.empty[A] 

  def freshTermName[C <: Context](c: C)(s: String) =
    c.universe.TermName(c.freshName(s))

}
