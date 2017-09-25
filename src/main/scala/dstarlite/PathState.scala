package dstarlite

class PathState(queue: PriorityQueue[Node],
                graph: Graph,
                position: Node,
                goal: Node,
                gVals: Map[Node, Float],
                rhsVals: Map[Node, Float],
                km: Int) {

  def this(newQueue: PriorityQueue[Node]) =
    this(newQueue, graph, position, goal, gVals, rhsVals, km)

  def this(newGVals: Map[Node, Float]) =
    this(queue, graph, position, goal, newGVals, rhsVals, km)

  def this(newQueue: PriorityQueue[Node], newGVals: Map[Node, Float]) =
    this(newQueue, graph, position, goal, newGVals, rhsVals, km)

  def heuristic(node: Node, position: Node): Int = {
    Math.abs((node.x - position.x) + (node.y - position.y))
  }

  def calculateKey(node: Node): Key = {
    val gRhs = Math.min(gVals(node), rhsVals(node))
    Key(gRhs + heuristic(node, position), gRhs)
  }

  def lowestCostNeighbour(node: Node): Node = {
    val nodeLookAhead = lookaheadCost(node)
    graph.neighbours(node).minBy(nodeLookAhead)
  }

  private def lookaheadCost(node: Node)(neighbour: Node) = {
    graph.cost(node, neighbour) + gVals(neighbour)
  }

  def calculateRHS(node: Node): Float = {
    gVals(node) + graph.cost(node, lowestCostNeighbour(node))
  }

  def updateNodes(nodes: List[Node]): PathState =
    nodes.foldLeft(this)((pathState, node) => pathState.updateNode(node))

  def updateNode(node: Node): PathState = {
    val newRhsVals = updateRhs(node)
    val newQueue = queue.remove(node)
    if (gVals(node) != newRhsVals(node))
      new PathState(
        newQueue.put((calculateKey(node), node)),
        graph, position, goal, gVals, newRhsVals, km
      )
    else new PathState(newQueue, graph, position, goal, gVals, newRhsVals, km)
  }

  private def updateRhs(node: Node): Map[Node, Float] = {
    if (node != goal) rhsVals.updated(node, calculateRHS(node))
    else rhsVals
  }

  private def queueWithNew(queue: PriorityQueue[Node], node: Node): PriorityQueue[Node] = {
    queue put ((calculateKey(node), node))
  }

  def computeShortestPath: PathState = {
    val kFirst = queue.firstKey
    if (kFirst >= calculateKey(position) || rhsVals(position) == gVals(position)) this
    else {
      val (topNode, queueNoTop) = queue.pop
      val kFirstNew = calculateKey(topNode)

      if (kFirst < kFirstNew)
        new PathState(queueWithNew(queueNoTop, topNode)).computeShortestPath
      else if (gVals(topNode) > rhsVals(topNode))
        new PathState(
          queueNoTop,
          gVals.updated(topNode, rhsVals(topNode))
        ).updateNodes(graph.neighbours(topNode))
         .computeShortestPath
      else
        new PathState(
          queueNoTop,
          gVals.updated(topNode, Float.PositiveInfinity)
        ).updateNodes(topNode :: graph.neighbours(topNode))
         .computeShortestPath
    }
  }

}
