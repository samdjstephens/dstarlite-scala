package dstarlite

import scala.collection.immutable.SortedSet



class PriorityQueue[T](items: SortedSet[(Key, T)]){
  type QueueItem = (Key, T)

  def put(item: QueueItem): PriorityQueue[T] =
    new PriorityQueue[T](items + item)

  def pop: (T, PriorityQueue[T]) =
    if (items.isEmpty) throw new NoSuchElementException("PriorityQueue empty!")
    else (items.head match { case (k, n) => n }, new PriorityQueue[T](items.tail))

  def firstKey: Key = items.head match { case (key, _) => key }

  def allItems: List[QueueItem] = items.toList

  def contains(item: T): Boolean = {
    def _itemInList(itemList: List[QueueItem]): Boolean = itemList match {
      case Nil => false
      case (_, it) :: rest => if (it == item) true else _itemInList(rest)
    }
    _itemInList(allItems)
  }
}

object PriorityQueue {
  implicit def orderedByKey[T]: Ordering[(Key, T)] = Ordering.by({ case (key, _) => key })
  def apply[T] = new PriorityQueue[T](SortedSet.empty[(Key, T)])
  def apply[T](item: (Key, T)) = new PriorityQueue[T](SortedSet(item))
  def apply[T](items: (Key, T)*) = new PriorityQueue[T](SortedSet(items: _*))
}