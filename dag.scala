import scala.collection.mutable

trait AdNode
case class DebugNode(msg: String) extends AdNode
case class TejNode[T](tej: Tej[T]) extends AdNode
case class TejOp[T](op: String, value: Tej[T]) extends AdNode

def graphShow(adNode: AdNode): String = adNode match
  case DebugNode(msg)   => msg
  case TejNode(tej)     => tej.toString
  case TejOp(op, value) => op

class DAG {
  private val adjacencyList: mutable.Map[AdNode, mutable.Set[AdNode]] =
    mutable.Map.empty

  inline def addAdNode(AdNode: AdNode): Unit = {
    if (!adjacencyList.contains(AdNode)) {
      adjacencyList(AdNode) = mutable.Set.empty
    }
  }

  inline def addNode(AdNode: AdNode): Unit = {
    addAdNode(AdNode)
  }

  inline def addStringNode(str: String): Unit = {
    val AdNode = DebugNode(str)
    addAdNode(AdNode)
  }

  inline def addTejNode[T](tej: Tej[T]): Unit = {
    val AdNode = TejNode(tej)
    addAdNode(AdNode)
  }

  inline def addOpNode[T](op: String, t: Tej[T]): Unit = {
    val AdNode = TejOp(op, t)
    addAdNode(AdNode)
  }

  inline def addEdge(from: AdNode, to: AdNode): Unit = {
    require(adjacencyList.contains(from), s"AdNode $from does not exist.")
    require(adjacencyList.contains(to), s"AdNode $to does not exist.")
    adjacencyList(from) += to
  }

  inline def addTedge[T](from: Tej[T], to: Tej[T]): Unit = {
    val fromNode = TejNode(from)
    val toNode = TejNode(to)
    addEdge(fromNode, toNode)
  }
  inline def addOpEdge[T](from: Tej[T], to: TejOp[T]): Unit = {
    val fromNode = TejNode(from)
    addEdge(fromNode, to)

  }

  inline def addSedge[T](from: String, to: String): Unit = {
    val fromNode = DebugNode(from)
    val toNode = DebugNode(to)
    addEdge(fromNode, toNode)
  }

  inline def removeNode(AdNode: AdNode): Unit = {
    adjacencyList -= AdNode
    adjacencyList.values.foreach(_ -= AdNode)
  }

  inline def removeEdge(from: AdNode, to: AdNode): Unit = {
    adjacencyList.get(from).foreach(_ -= to)
  }

  inline def removeSedge(from: String, to: String): Unit = {
    val fromNode = DebugNode(from)
    val toNode = DebugNode(to)
    adjacencyList.get(fromNode).foreach(_ -= toNode)
  }

  inline def neighbors(AdNode: AdNode): Set[AdNode] = {
    adjacencyList.getOrElse(AdNode, Set.empty).toSet
  }

  inline def AdNodes: Set[AdNode] = adjacencyList.keySet.toSet

  inline def hasEdge(from: AdNode, to: AdNode): Boolean = {
    adjacencyList.get(from).exists(_.contains(to))
  }

  inline def isEmpty: Boolean = adjacencyList.isEmpty

  inline def toposort: List[AdNode] = {
    val inDegree = mutable.Map[AdNode, Int]().withDefaultValue(0)
    val zeroInDegreeQueue = mutable.Queue[AdNode]()
    val sortedList = mutable.ListBuffer[AdNode]()

    // Initialize in-degree of each node
    adjacencyList.foreach { case (node, neighbors) =>
      neighbors.foreach { neighbor =>
        inDegree(neighbor) += 1
      }
    }

    // Enqueue nodes with zero in-degree
    adjacencyList.keys.foreach { node =>
      if (inDegree(node) == 0) {
        zeroInDegreeQueue.enqueue(node)
      }
    }

    // Process nodes with zero in-degree
    while (zeroInDegreeQueue.nonEmpty) {
      val node = zeroInDegreeQueue.dequeue()
      sortedList += node

      adjacencyList(node).foreach { neighbor =>
        inDegree(neighbor) -= 1
        if (inDegree(neighbor) == 0) {
          zeroInDegreeQueue.enqueue(neighbor)
        }
      }
    }

    // Check for cycles
    if (sortedList.size != adjacencyList.size) {
      throw new IllegalArgumentException("Graph has at least one cycle.")
    }

    sortedList.toList
  }

  inline def toGraphviz: String = {
    val sb = new StringBuilder
    sb.append("digraph {\n")

    adjacencyList.foreach { case (node, neighbors) =>
      if (neighbors.isEmpty) {
        sb.append(s"  \"${graphShow(node)}\";\n")
      } else {
        neighbors.foreach { neighbor =>
          sb.append(
            s"  \"${graphShow(node)}\" -> \"${graphShow(neighbor)}\";\n"
          )
        }
      }
    }

    sb.append("}")
    sb.toString()
  }
}
