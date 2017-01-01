package net.alasc.attributes

/** Base trait for objects that contain attributes in a dictionnary. Attributes are properties that are
  * difficult to compute, and/or for which the computation needs external resources or algorithms.
  * 
  * The attribute `a: Attribute` can be retrieved in a typesafe manner from an object `t: T` in two cases:
  * 
  * - an implicit instance of `a.Compute[T] ` is available,
  * - there is evidence of type `Known[t.type, a.type]` that the attribute `a` has already been computed for `t`.
  * 
  * Entries in the dictionary are never deleted, so any evidence produced is valid for the lifetime of the object.
  * 
  */
trait Attributable { self =>

  import Compat.{MutableLongMap, emptyMutableLongMap}

  private[this] var _attrDict: MutableLongMap[Any] = _

  def _attrGetOrUpdate[V](a: Attributes#Attribute)(v: => V)(implicit ev: Value[a.type, self.type, V]): V = synchronized {
    if (_attrDict eq null) _attrDict = emptyMutableLongMap[Any]
    if (!_attrDict.isDefinedAt(a.longKey)) {
      val computed = v
      _attrDict.put(a.longKey, computed)
      computed
    } else _attrDict(a.longKey).asInstanceOf[V]
  }

  def _attrGet[V](a: Attributes#Attribute)(implicit v: Value[a.type, self.type, V]): Option[V] = synchronized {
    if ((_attrDict ne null) && _attrDict.isDefinedAt(a.longKey))
      Some(_attrDict(a.longKey).asInstanceOf[V])
    else
      None
  }

  def _attrApply[V](a: Attributes#Attribute)(implicit v: Value[a.type, self.type, V]): V = synchronized {
    _attrDict(a.longKey).asInstanceOf[V]
  }


  def _attrIsDefined(a: Attributes#Attribute): Boolean = synchronized {
    if (_attrDict eq null) false else _attrDict.isDefinedAt(a.longKey)
  }

}
