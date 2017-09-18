package dstarlite

import scala.collection.immutable.SortedSet



class PriorityQueue[T](val items: SortedSet[(Key, T)]){
  type QueueItem = (Key, T)

  def put(item: QueueItem): PriorityQueue[T] =
    new PriorityQueue[T](items + item)

  def pop: (T, PriorityQueue[T]) =
    if (items.isEmpty) throw new NoSuchElementException("PriorityQueue empty!")
    else (items.head match { case (k, n) => n }, new PriorityQueue[T](items.tail))

  def firstKey: Key = items.head match { case (key, _) => key }

  def contains(node: T): Boolean = {
    def _itemInList(itemList: List[QueueItem]): Boolean = itemList match {
      case Nil => false
      case (_, it) :: rest => if (it == node) true else _itemInList(rest)
    }
    _itemInList(items.toList)
  }

  def remove(node: T): PriorityQueue[T] = {
    new PriorityQueue[T](items filter { case (_, it) => it != node })
  }

  def updateKey(node: T, newKey: Key): PriorityQueue[T] = {
    (this remove node) put (newKey, node)
  }

  def ==(other: PriorityQueue[T]): Boolean = {
    items == other.items
  }
}

object PriorityQueue {
  implicit def orderedByKey[T]: Ordering[(Key, T)] = Ordering.by({ case (key, _) => key })
  def apply[T] = new PriorityQueue[T](SortedSet.empty[(Key, T)])
  def apply[T](item: (Key, T)) = new PriorityQueue[T](SortedSet(item))
  def apply[T](items: (Key, T)*) = new PriorityQueue[T](SortedSet(items: _*))
}