package net.alasc.attributes

object Compat {

  type Context = scala.reflect.macros.Context

  type MutableLongMap[A] = scala.collection.mutable.HashMap[Long, A]

  def emptyMutableLongMap[A]: MutableLongMap[A] = scala.collection.mutable.HashMap.empty[Long, A] 

  def freshTermName[C <: Context](c: C)(s: String) =
    c.universe.newTermName(c.fresh(s))

}
