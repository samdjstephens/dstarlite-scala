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
}
