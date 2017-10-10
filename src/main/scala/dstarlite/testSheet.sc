import dstarlite._

val graph = new Graph(10, 10, Set.empty)
val start = Node(0, 5)
val goal = Node(7, 5)  // straight line shortest path
val rhsValsInit = Map((goal, 0.toFloat)).withDefaultValue(Float.PositiveInfinity)
rhsValsInit
val initialQueue = PriorityQueue[Node]()
val ps = new PathState(
  initialQueue,
  graph,
  start,
  goal,
  Map[Node, Float]().withDefaultValue(Float.PositiveInfinity),
  rhsValsInit,
  0
)
val goalInitialKey = ps.calculateKey(goal)

val computedPs = ps.withQueue(initialQueue.put((goalInitialKey, goal))).computeShortestPath

computedPs.getShortestPath