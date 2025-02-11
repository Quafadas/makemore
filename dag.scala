import scala.collection.mutable
import java.util.UUID
import java.{util => ju}

trait AdNode {
  inline def id: UUID
}
case class DebugNode(msg: String) extends AdNode {
  val n = UUID.randomUUID()
  override inline def id: UUID = n
}
case class TejNode[T](tej: Tej[T]) extends AdNode {
  override inline def id: UUID = tej.nodeId
}

case class TejOp[T](op: String, value: Tej[T]) extends AdNode {
  override inline def id: UUID = value.nodeId
}

def graphShow(adNode: AdNode): String = adNode match
  case DebugNode(msg) => msg
  case TejNode(tej)   => tej.toString
  case TejOp(op, value) =>
    s"$op \n $value \n (_id: ${(value.nodeId.toString().take(4))})"

class DAG {
  private val adjacencyList: mutable.Map[UUID, mutable.Set[UUID]] =
    mutable.Map.empty
  private val nodeMap: mutable.Map[UUID, AdNode] = mutable.Map.empty

  inline def addAdNode(adNode: AdNode): Unit = {
    if (!adjacencyList.contains(adNode.id)) {
      adjacencyList(adNode.id) = mutable.Set.empty
      nodeMap(adNode.id) = adNode
    }
  }

  inline def addNode(adNode: AdNode): Unit = {
    addAdNode(adNode)
  }

  inline def addStringNode(str: String): Unit = {
    val adNode = DebugNode(str)
    addAdNode(adNode)
  }

  inline def addTejNode[T](tej: Tej[T]): Unit = {
    val adNode = TejNode(tej)
    addAdNode(adNode)
  }

  inline def addOpNode[T](op: String, t: Tej[T]): Unit = {
    val adNode = TejOp(op, t)
    addAdNode(adNode)
  }

  inline def addEdge(from: AdNode, to: AdNode): Unit = {
    require(adjacencyList.contains(from.id), s"AdNode $from does not exist.")
    require(adjacencyList.contains(to.id), s"AdNode $to does not exist.")
    adjacencyList(from.id) += to.id
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

  inline def removeNode(adNode: AdNode): Unit = {
    adjacencyList -= adNode.id
    nodeMap -= adNode.id
    adjacencyList.values.foreach(_ -= adNode.id)
  }

  inline def removeEdge(from: AdNode, to: AdNode): Unit = {
    adjacencyList.get(from.id).foreach(_ -= to.id)
  }

  inline def removeSedge(from: String, to: String): Unit = {
    val fromNode = DebugNode(from)
    val toNode = DebugNode(to)
    adjacencyList.get(fromNode.id).foreach(_ -= toNode.id)
  }

  inline def neighbors(adNode: AdNode): Set[AdNode] = {
    adjacencyList.getOrElse(adNode.id, Set.empty).flatMap(nodeMap.get).toSet
  }

  inline def AdNodes: Set[AdNode] = nodeMap.values.toSet

  inline def hasEdge(from: AdNode, to: AdNode): Boolean = {
    adjacencyList.get(from.id).exists(_.contains(to.id))
  }

  inline def isEmpty: Boolean = adjacencyList.isEmpty

  inline def toposort: List[AdNode] = {
    val inDegree = mutable.Map[UUID, Int]().withDefaultValue(0)
    val zeroInDegreeQueue = mutable.Queue[UUID]()
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
      sortedList += nodeMap(node)

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
        sb.append(s"  \"${graphShow(nodeMap(node))}\";\n")
      } else {
        neighbors.foreach { neighbor =>
          sb.append(
            s"  \"${graphShow(nodeMap(node))}\" -> \"${graphShow(nodeMap(neighbor))}\";\n"
          )
        }
      }
    }

    sb.append("}")
    sb.toString()
  }
}
