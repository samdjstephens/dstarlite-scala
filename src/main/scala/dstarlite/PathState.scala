package dstarlite

class PathState(queue: PriorityQueue[Node],
                graph: Graph,
                position: Node,
                goal: Node,
                gVals: Map[Node, Float],
                rhsVals: Map[Node, Float],
                km: Int) {

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
        graph, position, goal, gVals, rhsVals, km
      )  // TODO: Can I use implicits to sort this out?
    else new PathState(newQueue, graph, position, goal, gVals, rhsVals, km)
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
        new PathState(
          queueWithNew(queueNoTop, topNode),
          graph, position, goal, gVals, rhsVals, km
        ).computeShortestPath
      else if (gVals(topNode) > rhsVals(topNode))
        new PathState(
          queueNoTop, graph, position, goal,
          gVals.updated(topNode, rhsVals(topNode)),
          rhsVals, km
        ).updateNodes(graph.neighbours(topNode))
          .computeShortestPath
      else
        new PathState(
          queueNoTop, graph, position, goal,
          gVals.updated(topNode, Float.PositiveInfinity),
          rhsVals, km
        ).updateNodes(topNode :: graph.neighbours(topNode))
          .computeShortestPath
    }
  }

}
