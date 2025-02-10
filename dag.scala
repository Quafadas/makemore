

import scala.collection.mutable

trait AdNode(val name: String)

class DAG[NODE <: AdNode | String] {
  private val adjacencyList: mutable.Map[NODE, mutable.Set[NODE]] = mutable.Map.empty

  def addNode(node: NODE): Unit = {
    if (!adjacencyList.contains(node)) {
      adjacencyList(node) = mutable.Set.empty
    }
  }

  def addEdge(from: NODE, to: NODE): Unit = {
    require(adjacencyList.contains(from), s"Node $from does not exist.")
    require(adjacencyList.contains(to), s"Node $to does not exist.")
    adjacencyList(from) += to
  }

  def removeNode(node: NODE): Unit = {
    adjacencyList -= node
    adjacencyList.values.foreach(_ -= node)
  }

  def removeEdge(from: NODE, to: NODE): Unit = {
    adjacencyList.get(from).foreach(_ -= to)
  }

  def neighbors(node: NODE): Set[NODE] = {
    adjacencyList.getOrElse(node, Set.empty).toSet
  }

  def nodes: Set[NODE] = adjacencyList.keySet.toSet

  def hasEdge(from: NODE, to: NODE): Boolean = {
    adjacencyList.get(from).exists(_.contains(to))
  }

  def isEmpty: Boolean = adjacencyList.isEmpty

  def toposort: List[NODE] = {
    val inDegree = mutable.Map[NODE, Int]().withDefaultValue(0)
    val zeroInDegreeQueue = mutable.Queue[NODE]()
    val sortedList = mutable.ListBuffer[NODE]()

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

  def toGraphviz: String = {  
    val sb = new StringBuilder  
    sb.append("digraph {\n")  
  
    adjacencyList.foreach { case (node, neighbors) =>  
      if (neighbors.isEmpty) {  
        sb.append(s"  \"$node\";\n")  
      } else {  
        neighbors.foreach { neighbor =>  
          sb.append(s"  \"$node\" -> \"$neighbor\";\n")  
        }  
      }  
    }  
  
    sb.append("}")  
    sb.toString()  
  }  
}
