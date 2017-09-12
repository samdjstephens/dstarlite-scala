package dstarlite
import org.scalatest._

class PriorityQueueSuite extends FlatSpec {
  def theQueue = PriorityQueue(
    (Key(1, 2), Node(1, 2)),
    (Key(1, 1), Node(1, 1)),
    (Key(2, 1), Node(2, 1))
  )

  "PriorityQueue.firstKey" must "return the smallest key in the set" in {
    val q = theQueue
    assert(q.firstKey == Key(1, 1))
  }

  it must "return the smallest key from a set derived by inserting a new key" in {
    val q = theQueue put (Key(2, 2), Node(2, 2))

    assert(q.firstKey == Key(1, 1))
  }

  it must "return the new key from a set derived by inserting a new key if that key is smallest" in {
    val q = theQueue put (Key(0, 1), Node(0, 1))

    assert(q.firstKey == Key(0, 1))
  }

  "PriorityQueue.pop" must "return & remove the smallest item in the set" in {
    val q = theQueue
    val (node, q2) = theQueue.pop
    assert(node == Node(1, 1))
    assert(!(q2 contains node))
  }

  it must "return & remove the smallest item from a set derived by inserting a new key" in {
    val q = theQueue put (Key(2, 2), Node(2, 2))
    val (node, q2) = q.pop
    assert(node == Node(1, 1))
    assert(!(q2 contains node))
  }

  it must "return & remove the new item from a set derived by inserting a new item if that item's key is smallest" in {
    val q = theQueue put (Key(0, 1), Node(0, 1))
    val (node, q2) = q.pop

    assert(node == Node(0, 1))
    assert(!(q2 contains node))
  }

  // TODO: Add test for contains
}
