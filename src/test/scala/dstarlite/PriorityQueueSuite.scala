package dstarlite
import org.scalatest._

class PriorityQueueSuite extends FlatSpec {
  "PriorityQueue.firstKey" must "return the smallest key in the set" in {
    val q = PriorityQueue(
      (Key(1, 2), Node(1, 2)),
      (Key(1, 1), Node(1, 1)),
      (Key(2, 1), Node(2, 1))
    )
    assert(q.firstKey == Key(1, 1))
  }

  it must "return the smallest key from a set derived by inserting a new key" in {
    val q = PriorityQueue(
      (Key(1, 2), Node(1, 2)),
      (Key(1, 1), Node(1, 1)),
      (Key(2, 1), Node(2, 1))
    ) put (Key(2, 2), Node(2, 2))

    assert(q.firstKey == Key(1, 1))
  }

  it must "return the new key from a set derived by inserting a new key if that key is smallest" in {
    val q = PriorityQueue(
      (Key(1, 2), Node(1, 2)),
      (Key(1, 1), Node(1, 1)),
      (Key(2, 1), Node(2, 1))
    ) put (Key(0, 1), Node(0, 1))

    assert(q.firstKey == Key(0, 1))
  }
}
