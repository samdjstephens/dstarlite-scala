package dstarlite

class Graph(height: Int, width: Int, walls: Set[Node]) {
  def inBounds(node: Node): Boolean = node match {
    case Node(x, y) =>
      if ((x < width) & (x < height) & (x >= 0) & (y >= 0) & !(walls contains node)) true
      else false
  }

  def cost(from: Node, to: Node): Float =
    if ((walls contains from) | (walls contains to)) Float.PositiveInfinity
    else 1

  def neighbours(node: Node): List[Node] = {
    val nodes = for {
      x <- node.x-1 until node.x+1
      y <- node.y-1 until node.y+1
      neighbour = Node(x, y)
      if inBounds(neighbour)
    } yield neighbour
    nodes.toList
  }
}
