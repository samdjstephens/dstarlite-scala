package dstarlite

import scala.collection.immutable.SortedSet



class PriorityQueue[T](items: SortedSet[(Int, T)]){
  type QueueItem = (Int, T)
  def put(item: QueueItem): PriorityQueue[T] =
    new PriorityQueue[T](items + item)
//  def pop = ???
  def firstKey: Int = items.head match { case (key, _) => key }
  def allItems: List[QueueItem] = items.toList
}

object PriorityQueue {
  implicit def orderedByKey[T]: Ordering[(Int, T)] = Ordering.by({ case (i, _) => i })
  def apply[T] = new PriorityQueue[T](SortedSet.empty[(Int, T)])
  def apply[T](item: (Int, T)) = new PriorityQueue[T](SortedSet(item))
}