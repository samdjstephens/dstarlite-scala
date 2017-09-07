package dstarlite
import org.scalatest._

class PriorityQueueSuite extends FlatSpec {
  "PriorityQueue.firstKey" must "return the key of the item inserted if it is smaller than the existing items" in {
    val node1 = Node(1, 2)
    val node2 = Node(2, 3)
    val q1 = PriorityQueue(2, node1)
    val q2 = q1 put ((1, node2))
    assert(q2.firstKey == 1)
  }
}
