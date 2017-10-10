package dstarlite

class PathState(queue: PriorityQueue[Node],
                graph: Graph,
                position: Node,
                goal: Node,
                gVals: Map[Node, Float],
                rhsVals: Map[Node, Float],
                km: Int) {

  def withQueue(newQueue: PriorityQueue[Node]) =
    new PathState(newQueue, graph, position, goal, gVals, rhsVals, km)

  def withGVals(newGVals: Map[Node, Float]) =
    new PathState(queue, graph, position, goal, newGVals, rhsVals, km)

  def withRHSVals(newRHSVals: Map[Node, Float]) =
    new PathState(queue, graph, position, goal, gVals, newRHSVals, km)

  def heuristic(node: Node, position: Node): Int = {
    Math.abs((node.x - position.x) + (node.y - position.y))
  }

  def calculateKey(node: Node): Key = {
    val gRhs = Math.min(gVals(node), rhsVals(node))
    Key(gRhs + heuristic(node, position), gRhs)
  }

  def lowestCostNeighbour(node: Node): Node = {
    graph.neighbours(node).minBy(lookaheadCost(node))
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
      this
        .withQueue(newQueue.put((calculateKey(node), node)))
        .withRHSVals(newRhsVals)
    else
      this
        .withQueue(newQueue)
        .withRHSVals(newRhsVals)
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
        this
          .withQueue(queueWithNew(queueNoTop, topNode))
          .computeShortestPath
      else if (gVals(topNode) > rhsVals(topNode))
        this
          .withQueue(queueNoTop)
          .withGVals(gVals.updated(topNode, rhsVals(topNode)))
          .updateNodes(graph.neighbours(topNode))
          .computeShortestPath
      else
        this
          .withQueue(queueNoTop)
          .withGVals(gVals.updated(topNode, Float.PositiveInfinity))
          .updateNodes(topNode :: graph.neighbours(topNode))
          .computeShortestPath
    }
  }

  def getShortestPath: List[Node] = {
    def recur(curPos: Node, path: List[Node], recurDepth: Int): List[Node] = {
      println(curPos)
      if (recurDepth == 20) throw new Exception("max recur depth exceeded")
      else if (curPos == goal) goal :: path
      else {
        val newPos = lowestCostNeighbour(curPos)
        recur(newPos, curPos :: path, recurDepth+1)
      }
    }
    recur(position, List.empty, 0)
  }
}