package dstarlite

import scala.collection.immutable.SortedSet



class PriorityQueue[T](items: SortedSet[(Key, T)]){
  type QueueItem = (Key, T)
  def put(item: QueueItem): PriorityQueue[T] =
    new PriorityQueue[T](items + item)
//  def pop = ???
  def firstKey: Key = items.head match { case (key, _) => key }
  def allItems: List[QueueItem] = items.toList
}

object PriorityQueue {
  implicit def orderedByKey[T]: Ordering[(Key, T)] = Ordering.by({ case (key, _) => key })
  def apply[T] = new PriorityQueue[T](SortedSet.empty[(Key, T)])
  def apply[T](item: (Key, T)) = new PriorityQueue[T](SortedSet(item))
  def apply[T](items: (Key, T)*) = new PriorityQueue[T](SortedSet(items: _*))
}